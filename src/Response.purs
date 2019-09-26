module Payload.Response where

import Prelude

import Control.Monad.Except (ExceptT(..))
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
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.Status (HttpStatus)
import Payload.Status as Status
import Payload.Utils as Utils
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals, to)
import Unsafe.Coerce (unsafeCoerce)

type Handler a = ExceptT ServerError Aff a

foreign import endResponse_ :: HTTP.Response -> Unit -> (Unit -> Effect Unit) -> Effect Unit

data UnsafeStream

endResponse :: HTTP.Response -> Aff Unit
endResponse res = Aff.makeAff \cb -> do
  endResponse_ res unit (\_ -> cb (Right unit))
  pure Aff.nonCanceler

newtype Json a = Json a
data Empty = Empty

type ServerError = String

newtype Response r = Response
  { status :: HttpStatus
  , headers :: Headers
  , body :: r }

derive instance newtypeResponse :: Newtype (Response a) _
instance eqResponse :: Eq a => Eq (Response a) where
  eq (Response r1) (Response r2) = r1 == r2
instance showResponse :: Show a => Show (Response a) where
  show (Response r) = show r

type RawResponse = Response ResponseBody

setEmptyBody :: forall r. Response r -> Response ResponseBody
setEmptyBody = over Response (_ { body = EmptyBody })

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

-- This type class is for converting between compatible types.
-- If the spec says one type is returned from an endpoint, a handler
-- can either return that type directly or return another type from
-- which that type can be produced (e.g. a full response with different
-- headers or a different status code).
class ToResponse a b where
  toResponse :: a -> Response b

instance toResponseResponse :: ToResponse (Response a) a where
  toResponse res = res
else instance toResponseIdentity :: ToResponse a a where
  toResponse res  =
    Response { status: Status.ok, headers: Headers.empty, body: res }

-- Types with in an instance for EncodeResponse are those that
-- can appear in the response field of an API spec and ultimately
-- can be encoded as one of the raw response types
class EncodeResponse r where
  encodeResponse :: Response r -> Handler RawResponse
instance encodeResponseResponseBody :: EncodeResponse ResponseBody where
  encodeResponse = pure
else instance encodeResponseRecord ::
  ( SimpleJson.WriteForeign (Record r)
  ) => EncodeResponse (Record r) where
  encodeResponse (Response r) = encodeResponse (Response $ r { body = Json r.body })
else instance encodeResponseArray ::
  ( SimpleJson.WriteForeign (Array r)
  ) => EncodeResponse (Array r) where
  encodeResponse (Response r) = encodeResponse (Response $ r { body = Json r.body })
else instance encodeResponseJson ::
  ( SimpleJson.WriteForeign r
  ) => EncodeResponse (Json r) where
  encodeResponse (Response r@{ body: Json json }) = pure $ Response $
        { status: r.status
        , headers: Headers.setIfNotDefined "content-type" ContentType.json r.headers
        , body: StringBody (SimpleJson.writeJSON json) }
else instance encodeResponseString :: EncodeResponse String where
  encodeResponse (Response r) = pure $ Response
                   { status: r.status
                   , headers: Headers.setIfNotDefined "content-type" ContentType.plain r.headers
                   , body: StringBody r.body }
else instance encodeResponseStream ::
  ( TypeEquals (Stream.Stream r) (Stream.Stream (read :: Stream.Read | r')))
  => EncodeResponse (Stream.Stream r) where
  encodeResponse (Response r) = pure $ Response
                   { status: r.status
                   , headers: Headers.setIfNotDefined "content-type" ContentType.plain r.headers
                   , body: StreamBody (unsafeCoerce r.body) }
else instance encodeResponseMaybe :: EncodeResponse a => EncodeResponse (Maybe a) where
  encodeResponse (Response { body: Nothing }) = pure $ Response
                   { status: Status.notFound
                   , headers: Headers.empty
                   , body: EmptyBody }
  encodeResponse (Response r@{ body: Just body }) = encodeResponse $ Response
                   { status: r.status
                   , headers: r.headers
                   , body }
else instance encodeResponseEmpty :: EncodeResponse Empty where
  encodeResponse (Response r) = pure $ Response
                   { status: r.status
                   , headers: r.headers
                   , body: EmptyBody }

else instance encodeResponseUnit :: EncodeResponse Unit where
  encodeResponse (Response r) = pure $ Response
                   { status: r.status
                   , headers: r.headers
                   , body: EmptyBody }

sendInternalError :: forall err. Show err => HTTP.Response -> err -> Aff Unit
sendInternalError res err = liftEffect $ sendError res (internalError (show err))

internalError :: String -> ErrorResponse
internalError body = { status: Status.internalServerError, body }

sendResponse :: HTTP.Response -> Either ServerError RawResponse -> Effect Unit
sendResponse res serverResult = Aff.runAff_ onComplete do
  liftEffect $ case serverResult of
    Right (Response serverRes) -> do
      HTTP.setStatusCode res serverRes.status.code
      HTTP.setStatusMessage res serverRes.status.reason
      case serverRes.body of
        StringBody str -> do
          let contentLength = show $ Encoding.byteLength str UTF8
          let headers = Headers.setIfNotDefined "content-length" contentLength serverRes.headers
          writeHeaders res headers
          writeStringBody res str
        StreamBody stream -> do
          writeHeaders res serverRes.headers
          writeStreamBody res stream
        EmptyBody -> do
          writeHeaders res serverRes.headers
          Aff.launchAff_ $ endResponse res
    Left errors -> sendError res { status: Status.internalServerError, body: (show errors) }
  where
    onComplete (Left errors) = sendError res (internalError (show errors))
    onComplete (Right _) = pure unit

writeHeaders :: HTTP.Response -> Headers -> Effect Unit
writeHeaders res headers = do
  let (sets :: Array (Effect Unit)) = map (\(Tuple k v) -> HTTP.setHeader res k v) (Headers.toUnfoldable headers)
  sequence_ sets

writeStringBody :: HTTP.Response -> String -> Effect Unit
writeStringBody res str = do
  let out = HTTP.responseAsStream res
  _ <- Stream.writeString out UTF8 str (pure unit)
  Stream.end out (pure unit)

writeStreamBody :: HTTP.Response -> UnsafeStream -> Effect Unit
writeStreamBody res stream = do
  _ <- Stream.pipe (to (unsafeCoerce stream)) (HTTP.responseAsStream res)
  pure unit

type ErrorResponse =
  { body :: String
  , status :: Status.HttpStatus }

sendError
  :: HTTP.Response
  -> ErrorResponse
  -> Effect Unit
sendError res {body, status} = do
  let outputStream = HTTP.responseAsStream res
  HTTP.setHeader res "content-type" ContentType.plain
  HTTP.setHeader res "content-length" (show $ Encoding.byteLength body UTF8)
  HTTP.setStatusCode res status.code
  HTTP.setStatusMessage res status.reason
  _ <- Stream.writeString outputStream UTF8 body (pure unit)
  Stream.end outputStream (pure unit)

status :: forall a. HttpStatus -> a -> Response a
status s body = Response { status: s, headers: Headers.empty, body }
