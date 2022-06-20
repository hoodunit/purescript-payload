module Payload.Server.Internal.ServerResponse where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.ResponseTypes (RawResponse, Response(..), ResponseBody(..), UnsafeStream)
import Payload.Server.Response (internalError)
import Type.Equality (to)
import Unsafe.Coerce (unsafeCoerce)

sendResponse :: HTTP.Response -> RawResponse -> Effect Unit
sendResponse res rawResp = Aff.runAff_ onComplete do
  liftEffect (writeResponse res rawResp)
  where
    onComplete (Left errors) = do
      log $ "Error sending response:\n  Server response:\n" <> show rawResp <>
        "\n\n  Error(s): " <> show errors
      writeResponse res (internalError (StringBody "Error sending server response"))
    onComplete (Right _) = pure unit

writeResponse :: HTTP.Response -> RawResponse -> Effect Unit
writeResponse res (Response serverRes) = do
  HTTP.setStatusCode res serverRes.status.code
  HTTP.setStatusMessage res serverRes.status.reason
  writeBodyAndHeaders res serverRes.headers serverRes.body

writeBodyAndHeaders :: HTTP.Response -> Headers -> ResponseBody -> Effect Unit
writeBodyAndHeaders res headers (StringBody str) = do
  let contentLength = show $ Encoding.byteLength str UTF8
  let newHeaders = Headers.setIfNotDefined "content-length" contentLength headers
  writeHeaders res newHeaders
  writeStringBody res str
writeBodyAndHeaders res headers (StreamBody stream) = do
  writeHeaders res headers
  writeStreamBody res stream
writeBodyAndHeaders res headers EmptyBody = do
  writeHeaders res headers
  Aff.launchAff_ $ endResponse res

foreign import endResponse_ :: HTTP.Response -> Unit -> (Unit -> Effect Unit) -> Effect Unit

endResponse :: HTTP.Response -> Aff Unit
endResponse res = Aff.makeAff \cb -> do
  endResponse_ res unit (\_ -> cb (Right unit))
  pure Aff.nonCanceler

writeHeaders :: HTTP.Response -> Headers -> Effect Unit
writeHeaders res headers = do
  let (sets :: Array (Effect Unit)) = map (\(Tuple k v) -> HTTP.setHeader res k v) (Headers.toUnfoldable headers)
  sequence_ sets

writeStringBody :: HTTP.Response -> String -> Effect Unit
writeStringBody res str = do
  let out = HTTP.responseAsStream res
  _ <- Stream.writeString out UTF8 str (const $ pure unit)
  Stream.end out (const $ pure unit)

writeStreamBody :: HTTP.Response -> UnsafeStream -> Effect Unit
writeStreamBody res stream = do
  _ <- Stream.pipe (to (unsafeCoerce stream)) (HTTP.responseAsStream res)
  pure unit
