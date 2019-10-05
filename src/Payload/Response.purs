module Payload.Response where

import Prelude

import Control.Monad.Except (ExceptT(..), throwError)
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

type Result a = ExceptT Failure Aff a
data Failure = Forward String | ServerError RawResponse

instance showFailure :: Show Failure where
  show (Forward s) = "Forward '" <> s <> "'"
  show (ServerError e) = "ServerError " <> show e

foreign import endResponse_ :: HTTP.Response -> Unit -> (Unit -> Effect Unit) -> Effect Unit

data UnsafeStream

endResponse :: HTTP.Response -> Aff Unit
endResponse res = Aff.makeAff \cb -> do
  endResponse_ res unit (\_ -> cb (Right unit))
  pure Aff.nonCanceler

newtype Json a = Json a
data Empty = Empty

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
  toResponse :: a -> Result (Response b)

instance toResponseResponse :: ToResponse (Response a) a where
  toResponse res = pure res
else instance toResponseEitherStringVal :: ToResponse (Either String a) a where
  toResponse (Left res) = throwError (internalError_ res)
  toResponse (Right res) = pure (ok res)
else instance toResponseEitherStringResp :: ToResponse (Either String (Response a)) a where
  toResponse (Left res) = throwError (internalError_ res)
  toResponse (Right res) = pure res
else instance toResponseEitherFailurerVal :: ToResponse (Either Failure a) a where
  toResponse (Left err) = throwError err
  toResponse (Right res) = pure (ok res)
else instance toResponseEitherFailureResponse ::
  ToResponse (Either Failure (Response a)) a where
  toResponse (Left err) = throwError err
  toResponse (Right res) = pure res
else instance toResponseEitherResponseResponse ::
  ToResponse (Either (Response ResponseBody) (Response a)) a where
  toResponse (Left res) = throwError (ServerError res)
  toResponse (Right res) = pure res
else instance toResponseEitherResponseVal ::
  ToResponse (Either (Response ResponseBody) a) a where
  toResponse (Left res) = throwError (ServerError res)
  toResponse (Right res) = pure (ok res)
else instance toResponseIdentity :: ToResponse a a where
  toResponse res = pure (ok res)

-- Types with in an instance for EncodeResponse are those that
-- can appear in the response field of an API spec and ultimately
-- can be encoded as one of the raw response types
class EncodeResponse r where
  encodeResponse :: Response r -> Result RawResponse
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
sendInternalError res err = liftEffect $ writeResponse res (internalError (show err))

sendResponse :: HTTP.Response -> Either RawResponse RawResponse -> Effect Unit
sendResponse res serverResult = Aff.runAff_ onComplete do
  liftEffect $ case serverResult of
    Right serverRes -> writeResponse res serverRes
    Left err -> writeResponse res err
  where
    onComplete (Left errors) = writeResponse res (internalError (show errors))
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

serverError :: HttpStatus -> String -> Failure
serverError s msg = ServerError (status s (StringBody msg))

internalError_ :: String -> Failure
internalError_ msg = ServerError $ status Status.internalServerError (StringBody msg)

internalError :: String -> RawResponse
internalError msg = status Status.internalServerError (StringBody msg)

status :: forall a. HttpStatus -> a -> Response a
status s body = Response { status: s, headers: Headers.empty, body }

ok :: forall a. a -> Response a
ok body = Response { status: Status.ok, headers: Headers.empty, body }
