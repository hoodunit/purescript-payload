module Payload.Response where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.Encoding as Encoding
import Node.HTTP as HTTP
import Node.Stream as Stream
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals, to)
import Unsafe.Coerce (unsafeCoerce)

type HttpStatus = Int
type ServerError = String

newtype Response r = Response
  { status :: HttpStatus
  , headers :: Map String String
  , body :: ResponseBody r }

data ResponseBody r = StringBody String | StreamBody (Stream.Readable r) | EmptyBody


class IsRespondable r where
  mkResponse :: forall s. r -> Aff (Either ServerError (Response s))

instance isRespondableResponse :: IsRespondable (Response a) where
  mkResponse r = pure $ Right (unsafeCoerce r)

instance isRespondableString :: IsRespondable String where
  mkResponse s = pure $ Right $ Response
                   { status: 200
                   , headers: Map.fromFoldable [ Tuple "Content-Type" "text/plain" ]
                   , body: StringBody s }

instance isRespondableStream ::
  ( TypeEquals (Stream.Stream r) (Stream.Stream (read :: Stream.Read | r'))
  , IsResponseBody (Stream.Stream r)
  ) => IsRespondable (Stream.Stream r) where
  mkResponse s = pure $ Right $ Response
                   { status: 200
                   , headers: Map.fromFoldable [ Tuple "Content-Type" "text/plain" ]
                   , body: StreamBody (unsafeCoerce s) }

instance isRespondableRecord ::
  ( SimpleJson.WriteForeign (Record r)
  ) => IsRespondable (Record r) where
  mkResponse record = pure $ Right $ Response
                        { status: 200
                        , headers: Map.fromFoldable [ Tuple "Content-Type" "application/json" ]
                        , body: StringBody (SimpleJson.writeJSON record) }

instance isRespondableArray ::
  ( SimpleJson.WriteForeign (Array r)
  ) => IsRespondable (Array r) where
  mkResponse arr = pure $ Right $ Response
                     { status: 200
                     , headers: Map.fromFoldable [ Tuple "Content-Type" "application/json" ]
                     , body: StringBody (SimpleJson.writeJSON arr) }

class IsResponseBody body where
  writeBody :: HTTP.Response -> body -> Effect Unit

instance isResponseBodyString :: IsResponseBody String where
  writeBody res str = do
    let out = HTTP.responseAsStream res
    _ <- Stream.writeString out UTF8 str (pure unit)
    Stream.end out (pure unit)

instance isResponseBodyStream ::
  ( TypeEquals (Stream.Stream r) (Stream.Stream (read :: Stream.Read | r'))
  ) => IsResponseBody (Stream.Stream r) where
  writeBody res stream = do
    _ <- Stream.pipe (to stream) (HTTP.responseAsStream res)
    pure unit

sendInternalError :: forall err. Show err => HTTP.Response -> err -> Aff Unit
sendInternalError res err = liftEffect $ sendError res (internalError (show err))

internalError :: String -> ErrorResponse
internalError body = { status: 500, statusMsg: "Internal error", body }

sendResponse :: forall res. IsRespondable res => HTTP.Response -> res -> Effect Unit
sendResponse res handlerRes = Aff.runAff_ onComplete do
  serverResult <- mkResponse handlerRes
  liftEffect $ case serverResult of
    Right (Response serverRes@{ body: StringBody str }) -> do
      let contentLengthHdr = Tuple "Content-Length" (show $ Encoding.byteLength str UTF8)
      let defaultHeaders = Map.fromFoldable [ contentLengthHdr ]
      let headers = serverRes.headers <> defaultHeaders
      HTTP.setStatusCode res serverRes.status
      writeHeaders res headers
      writeBody res str
    Right (Response serverRes@{ body: StreamBody stream }) -> do
      HTTP.setStatusCode res serverRes.status
      writeHeaders res serverRes.headers
      writeBody res stream
    Right (Response serverRes@{ body: EmptyBody }) -> do
      HTTP.setStatusCode res serverRes.status
      writeHeaders res serverRes.headers
    Left errors -> sendError res { status: 500, statusMsg: "Error encoding response", body: (show errors) }
  where
    onComplete (Left errors) = sendError res (internalError (show errors))
    onComplete (Right _) = pure unit

writeHeaders :: HTTP.Response -> Map String String -> Effect Unit
writeHeaders res headers = sequence_ $ Map.values $ mapWithIndex (HTTP.setHeader res) headers

type ErrorResponse =
  { body :: String
  , status :: Int
  , statusMsg :: String }

sendError
  :: HTTP.Response
  -> ErrorResponse
  -> Effect Unit
sendError res {body, status, statusMsg} = do
  let outputStream = HTTP.responseAsStream res
  HTTP.setHeader res "Content-Type" "text/plain"
  HTTP.setHeader res "Content-Length" (show $ Encoding.byteLength body UTF8)
  HTTP.setStatusCode res status
  HTTP.setStatusMessage res statusMsg
  _ <- Stream.writeString outputStream UTF8 body (pure unit)
  Stream.end outputStream (pure unit)
