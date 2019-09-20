module Payload.Response where

import Prelude

import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
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
import Payload.ContentType as ContentType
import Payload.Status (HttpStatus)
import Payload.Status as Status
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals, to)
import Unsafe.Coerce (unsafeCoerce)

foreign import endResponse_ :: HTTP.Response -> Unit -> (Unit -> Effect Unit) -> Effect Unit

data UnsafeStream

endResponse :: HTTP.Response -> Aff Unit
endResponse res = Aff.makeAff \cb -> do
  endResponse_ res unit (\_ -> cb (Right unit))
  pure Aff.nonCanceler

newtype Json a = Json a
data Status a = Status HttpStatus a
data Empty = Empty
data SetHeaders a = SetHeaders (Map String String) a

type ServerError = String

newtype Response r = Response
  { status :: HttpStatus
  , headers :: Map String String
  , response :: r }

newtype RawResponse = RawResponse
  { status :: HttpStatus
  , headers :: Map String String
  , body :: ResponseBody }

derive instance newtypeRawResponse :: Newtype RawResponse _
instance eqRawResponse :: Eq RawResponse where
  eq (RawResponse r1) (RawResponse r2) = r1 == r2
instance showRawResponse :: Show RawResponse where
  show (RawResponse r) = show r

setEmptyBody :: RawResponse -> RawResponse
setEmptyBody = over RawResponse (_ { body = EmptyBody })

data ResponseBody = StringBody String | StreamBody UnsafeStream | EmptyBody

instance eqResponseBody :: Eq ResponseBody where
  eq (StringBody s1) (StringBody s2) = s1 == s2
  eq EmptyBody EmptyBody = true
  eq (StreamBody _) (StreamBody _) = false
  eq _ _ = false
instance showResponseBody :: Show ResponseBody where
  show (StringBody s) = s
  show EmptyBody = "EmptyBody"
  show (StreamBody _) = "StreamBody"

class Responder r where
  mkResponse :: r -> Aff (Either ServerError RawResponse)

instance responderRawResponse :: Responder RawResponse where
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
                   , headers: Map.fromFoldable [ Tuple "Content-Type" ContentType.plain ]
                   , body: StringBody s }

instance responderStream ::
  ( TypeEquals (Stream.Stream r) (Stream.Stream (read :: Stream.Read | r'))
  , IsResponseBody (Stream.Stream r)
  ) => Responder (Stream.Stream r) where
  mkResponse s = pure $ Right $ RawResponse
                   { status: Status.ok
                   , headers: Map.fromFoldable [ Tuple "Content-Type" ContentType.plain ]
                   , body: StreamBody (unsafeCoerce s) }

instance responderStatus :: Responder a => Responder (Status a) where
  mkResponse (Status status inner) = do
    innerResult <- mkResponse inner
    case innerResult of
      Right (RawResponse { headers, body }) -> do
        pure $ Right $ RawResponse {status, headers, body}
      Left err -> pure $ Left err

instance responderRecord ::
  ( SimpleJson.WriteForeign (Record r)
  ) => Responder (Record r) where
  mkResponse record = pure $ Right $ RawResponse
                        { status: Status.ok
                        , headers: Map.fromFoldable [ Tuple "Content-Type" ContentType.json ]
                        , body: StringBody (SimpleJson.writeJSON record) }

instance responderArray ::
  ( SimpleJson.WriteForeign (Array r)
  ) => Responder (Array r) where
  mkResponse arr = pure $ Right $ RawResponse
                     { status: Status.ok
                     , headers: Map.fromFoldable [ Tuple "Content-Type" ContentType.json ]
                     , body: StringBody (SimpleJson.writeJSON arr) }

instance responderJson ::
  ( SimpleJson.WriteForeign r
  ) => Responder (Json r) where
  mkResponse (Json content) =
    pure $ Right $ RawResponse
      { status: Status.ok
      , headers: Map.fromFoldable [ Tuple "Content-Type" ContentType.json ]
      , body: StringBody (SimpleJson.writeJSON content) }

instance responderMaybe :: Responder a => Responder (Maybe a) where
  mkResponse Nothing =
    pure $ Right $ RawResponse { status: Status.notFound, headers: Map.empty, body: EmptyBody }
  mkResponse (Just r) = mkResponse r

instance responderEmpty :: Responder Empty where
  mkResponse s = pure $ Right $ RawResponse
                   { status: Status.ok
                   , headers: Map.empty
                   , body: EmptyBody }

instance responderUnit :: Responder Unit where
  mkResponse _ = pure $ Right $ RawResponse
                   { status: Status.ok
                   , headers: Map.empty
                   , body: EmptyBody }

instance responderSetHeaders :: Responder a => Responder (SetHeaders a) where
  mkResponse (SetHeaders newHeaders inner) = do
    innerResult <- mkResponse inner
    case innerResult of
      Right (RawResponse {status, headers, body}) -> do
        pure $ Right $ RawResponse
          { status: Status.ok
          , headers: Map.union newHeaders headers
          , body: EmptyBody }
      Left err -> pure $ Left $ err

class IsResponseBody body where
  writeBody :: HTTP.Response -> body -> Effect Unit

instance isResponseBodyString :: IsResponseBody String where
  writeBody res str = do
    let out = HTTP.responseAsStream res
    _ <- Stream.writeString out UTF8 str (pure unit)
    Stream.end out (pure unit)

instance isResponseBodyStream :: IsResponseBody UnsafeStream where
  writeBody res stream = do
    _ <- Stream.pipe (to (unsafeCoerce stream)) (HTTP.responseAsStream res)
    pure unit

sendInternalError :: forall err. Show err => HTTP.Response -> err -> Aff Unit
sendInternalError res err = liftEffect $ sendError res (internalError (show err))

internalError :: String -> ErrorResponse
internalError body = { status: Status.internalServerError, body }

sendResponse :: forall s. HTTP.Response -> Either ServerError RawResponse -> Effect Unit
sendResponse res serverResult = Aff.runAff_ onComplete do
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
      Aff.launchAff_ $ endResponse res
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
  HTTP.setHeader res "Content-Type" ContentType.plain
  HTTP.setHeader res "Content-Length" (show $ Encoding.byteLength body UTF8)
  HTTP.setStatusCode res status.code
  HTTP.setStatusMessage res status.reason
  _ <- Stream.writeString outputStream UTF8 body (pure unit)
  Stream.end outputStream (pure unit)
