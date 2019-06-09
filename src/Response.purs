module Payload.Response where

import Prelude

import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
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
import Payload.Status (HttpStatus)
import Payload.Status as Status
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals, to)
import Unsafe.Coerce (unsafeCoerce)

type ServerError = String

newtype Response r = Response
  { status :: HttpStatus
  , headers :: Map String String
  , response :: r }

newtype RawResponse r = RawResponse
  { status :: HttpStatus
  , headers :: Map String String
  , body :: ResponseBody r }

derive instance newtypeRawResponse :: Newtype (RawResponse r) _
instance eqRawResponse :: Eq (RawResponse r) where
  eq (RawResponse r1) (RawResponse r2) = r1 == r2
instance showRawResponse :: Show (RawResponse r) where
  show (RawResponse r) = show r

data ResponseBody r = StringBody String | StreamBody (Stream.Readable r) | EmptyBody

instance eqResponseBody :: Eq (ResponseBody r) where
  eq (StringBody s1) (StringBody s2) = s1 == s2
  eq EmptyBody EmptyBody = true
  eq (StreamBody _) (StreamBody _) = false
  eq _ _ = false
instance showResponseBody :: Show (ResponseBody r) where
  show (StringBody s) = s
  show EmptyBody = "EmptyBody"
  show (StreamBody _) = "StreamBody"

class Responder r where
  mkResponse :: forall s. r -> Aff (Either ServerError (RawResponse s))

instance responderRawResponse :: Responder (RawResponse a) where
  mkResponse r = pure $ Right (unsafeCoerce r)

instance responderResponse :: (Responder a) => Responder (Response a) where
  mkResponse (Response {status, headers, response}) = do
    innerRespResult <- mkResponse response
    case innerRespResult of
      Right (RawResponse innerResp) -> do
        pure $ Right $ RawResponse $ { status, headers, body: innerResp.body }
      Left err -> pure $ Left err

instance responderString :: Responder String where
  mkResponse s = pure $ Right $ RawResponse
                   { status: Status.ok
                   , headers: Map.fromFoldable [ Tuple "Content-Type" "text/plain" ]
                   , body: StringBody s }

instance responderStream ::
  ( TypeEquals (Stream.Stream r) (Stream.Stream (read :: Stream.Read | r'))
  , IsResponseBody (Stream.Stream r)
  ) => Responder (Stream.Stream r) where
  mkResponse s = pure $ Right $ RawResponse
                   { status: Status.ok
                   , headers: Map.fromFoldable [ Tuple "Content-Type" "text/plain" ]
                   , body: StreamBody (unsafeCoerce s) }

instance responderRecord ::
  ( SimpleJson.WriteForeign (Record r)
  ) => Responder (Record r) where
  mkResponse record = pure $ Right $ RawResponse
                        { status: Status.ok
                        , headers: Map.fromFoldable [ Tuple "Content-Type" "application/json" ]
                        , body: StringBody (SimpleJson.writeJSON record) }

instance responderArray ::
  ( SimpleJson.WriteForeign (Array r)
  ) => Responder (Array r) where
  mkResponse arr = pure $ Right $ RawResponse
                     { status: Status.ok
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
internalError body = { status: Status.internalServerError, body }

sendResponse :: forall res. Responder res => HTTP.Response -> res -> Effect Unit
sendResponse res handlerRes = Aff.runAff_ onComplete do
  serverResult <- mkResponse handlerRes
  liftEffect $ case serverResult of
    Right (RawResponse serverRes@{ body: StringBody str }) -> do
      let contentLengthHdr = Tuple "Content-Length" (show $ Encoding.byteLength str UTF8)
      let defaultHeaders = Map.fromFoldable [ contentLengthHdr ]
      let headers = serverRes.headers <> defaultHeaders
      HTTP.setStatusCode res serverRes.status.code
      HTTP.setStatusMessage res serverRes.status.reason
      writeHeaders res headers
      writeBody res str
    Right (RawResponse serverRes@{ body: StreamBody stream }) -> do
      HTTP.setStatusCode res serverRes.status.code
      HTTP.setStatusMessage res serverRes.status.reason
      writeHeaders res serverRes.headers
      writeBody res stream
    Right (RawResponse serverRes@{ body: EmptyBody }) -> do
      HTTP.setStatusCode res serverRes.status.code
      HTTP.setStatusMessage res serverRes.status.reason
      writeHeaders res serverRes.headers
    Left errors -> sendError res { status: Status.internalServerError, body: (show errors) }
  where
    onComplete (Left errors) = sendError res (internalError (show errors))
    onComplete (Right _) = pure unit

writeHeaders :: HTTP.Response -> Map String String -> Effect Unit
writeHeaders res headers = sequence_ $ Map.values $ mapWithIndex (HTTP.setHeader res) headers

type ErrorResponse =
  { body :: String
  , status :: Status.HttpStatus }

sendError
  :: HTTP.Response
  -> ErrorResponse
  -> Effect Unit
sendError res {body, status} = do
  let outputStream = HTTP.responseAsStream res
  HTTP.setHeader res "Content-Type" "text/plain"
  HTTP.setHeader res "Content-Length" (show $ Encoding.byteLength body UTF8)
  HTTP.setStatusCode res status.code
  HTTP.setStatusMessage res status.reason
  _ <- Stream.writeString outputStream UTF8 body (pure unit)
  Stream.end outputStream (pure unit)
