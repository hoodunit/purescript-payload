module Payload.Response
       -- ( class EncodeResponse
       -- , encodeResponse
       -- , class ToSpecResponse
       -- , toSpecResponse
       -- , Json(Json)
       -- , Empty(Empty)
       -- , RawResponse
       -- , Response(Response)
       -- , ResponseBody(StringBody, StreamBody, EmptyBody)
       -- , Result
       -- , Failure(Forward, ServerError)
       -- , UnsafeStream

       -- , internalError
       -- , internalError_
       -- , sendResponse
       -- , serverError
       -- , setEmptyBody
       -- , writeResponse

       -- , status
       -- , setStatus
       -- , updateStatus
       -- , setBody
       -- , updateBody
       -- , setHeaders
       -- , updateHeaders
       -- ) where
       where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Data.Either (Either(..))
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
import Payload.Internal.TypeErrors (type (<>), type (|>))
import Payload.Status (HttpStatus)
import Payload.Status as Status
import Prim.TypeError (class Fail, Quote, Text)
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals, to)
import Unsafe.Coerce (unsafeCoerce)

type Result a = ExceptT Failure Aff a
data Failure = Forward String | Error RawResponse

instance showFailure :: Show Failure where
  show (Forward s) = "Forward '" <> s <> "'"
  show (Error e) = "Error " <> show e

data UnsafeStream

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

-- | This type class is for converting types which are compatible with
-- | the spec into the spec type.
-- | If the spec says one type is returned from an endpoint, a handler
-- | can either return that type directly or return another type from
-- | which that type can be produced (e.g. a full response with different
-- | headers or a different status code).
class ToSpecResponse a b where
  toSpecResponse :: a -> Result (Response b)

instance toSpecResponseEitherStringVal
  :: EncodeResponse a
  => ToSpecResponse (Either String a) a where
  toSpecResponse (Left res) = throwError (Error $ internalError $ StringBody res)
  toSpecResponse (Right res) = pure (ok res)
else instance toSpecResponseEitherStringResp
  :: EncodeResponse a
  => ToSpecResponse (Either String (Response a)) a where
  toSpecResponse (Left res) = throwError (Error $ internalError $ StringBody res)
  toSpecResponse (Right res) = pure res
else instance toSpecResponseEitherFailurerVal
  :: EncodeResponse a
  => ToSpecResponse (Either Failure a) a where
  toSpecResponse (Left err) = throwError err
  toSpecResponse (Right res) = pure (ok res)
else instance toSpecResponseEitherFailureResponse
  :: EncodeResponse a
  => ToSpecResponse (Either Failure (Response a)) a where
  toSpecResponse (Left err) = throwError err
  toSpecResponse (Right res) = pure res
else instance toSpecResponseEitherResponseVal
  :: EncodeResponse err
  => ToSpecResponse (Either (Response err) a) a where
  toSpecResponse (Left res) = do
    raw <- encodeResponse res
    throwError (Error raw) 
  toSpecResponse (Right res) = pure (ok res)
else instance toSpecResponseEitherResponseResponse
  :: EncodeResponse err
  => ToSpecResponse (Either (Response err) (Response a)) a where
  toSpecResponse (Left res) = do
    raw <- encodeResponse res
    throwError (Error raw) 
  toSpecResponse (Right res) = pure res
else instance toSpecResponseResponse
  :: EncodeResponse a
  => ToSpecResponse (Response a) a where
  toSpecResponse res = pure res
else instance toSpecResponseIdentity
  :: EncodeResponse a
  => ToSpecResponse a a where
  toSpecResponse res = pure (ok res)
else instance toSpecResponseFail ::
  ( Fail (Text "Could not match spec response type with handler response type."
          |> Text ""
          |> Text "Spec requires:   " <> Quote b
          |> Text "Handler returns: " <> Quote a
          |> Text ""
          |> Text "No conversion from handler response to spec response was found."
          |> Text ""
          |> Text "Specifically, no type class instance was found for"
          |> Text ""
          |> Text "ToSpecResponse " <> Quote a
          |> Text "               " <> Quote b
          |> Text ""
         )
  ) => ToSpecResponse a b where
  toSpecResponse res = unsafeCoerce res

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

sendResponse :: HTTP.Response -> Either RawResponse RawResponse -> Effect Unit
sendResponse res serverResult = Aff.runAff_ onComplete do
  liftEffect $ case serverResult of
    Right serverRes -> writeResponse res serverRes
    Left err -> writeResponse res err
  where
    onComplete (Left errors) = writeResponse res (internalError $ StringBody $ show errors)
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
  _ <- Stream.writeString out UTF8 str (pure unit)
  Stream.end out (pure unit)

writeStreamBody :: HTTP.Response -> UnsafeStream -> Effect Unit
writeStreamBody res stream = do
  _ <- Stream.pipe (to (unsafeCoerce stream)) (HTTP.responseAsStream res)
  pure unit

status :: forall a. HttpStatus -> a -> Response a
status s body = Response { status: s, headers: Headers.empty, body }

setStatus :: forall a. HttpStatus -> Response a -> Response a
setStatus s = over Response (_ { status = s })

updateStatus :: forall a. (HttpStatus -> HttpStatus) -> Response a -> Response a
updateStatus f (Response res) = Response (res { status = f res.status })

setBody :: forall a b. b -> Response a -> Response b
setBody body = over Response (_ { body = body })

updateBody :: forall a b. (a -> b) -> Response a -> Response b
updateBody f (Response res) = Response (res { body = f res.body })

setHeaders :: forall a. Headers -> Response a -> Response a
setHeaders headers = over Response (_ { headers = headers })

updateHeaders :: forall a. (Headers -> Headers) -> Response a -> Response a
updateHeaders f (Response res) = Response (res { headers = f res.headers })


continue :: forall a. a -> Response a
continue = status Status.continue

switchingProtocols :: forall a. a -> Response a
switchingProtocols = status Status.switchingProtocols

processing :: forall a. a -> Response a
processing = status Status.processing

ok :: forall a. a -> Response a
ok = status Status.ok

created :: forall a. a -> Response a
created = status Status.created

accepted :: forall a. a -> Response a
accepted = status Status.accepted

nonAuthoritativeInformation :: forall a. a -> Response a
nonAuthoritativeInformation = status Status.nonAuthoritativeInformation

noContent :: forall a. a -> Response a
noContent = status Status.noContent

resetContent :: forall a. a -> Response a
resetContent = status Status.resetContent

partialContent :: forall a. a -> Response a
partialContent = status Status.partialContent

multiStatus :: forall a. a -> Response a
multiStatus = status Status.multiStatus

alreadyReported :: forall a. a -> Response a
alreadyReported = status Status.alreadyReported

imUsed :: forall a. a -> Response a
imUsed = status Status.imUsed

multipleChoices :: forall a. a -> Response a
multipleChoices = status Status.multipleChoices

movedPermanently :: forall a. a -> Response a
movedPermanently = status Status.movedPermanently

found :: forall a. a -> Response a
found = status Status.found

seeOther :: forall a. a -> Response a
seeOther = status Status.seeOther

notModified :: forall a. a -> Response a
notModified = status Status.notModified

useProxy :: forall a. a -> Response a
useProxy = status Status.useProxy

temporaryRedirect :: forall a. a -> Response a
temporaryRedirect = status Status.temporaryRedirect

permanentRedirect :: forall a. a -> Response a
permanentRedirect = status Status.permanentRedirect

badRequest :: forall a. a -> Response a
badRequest = status Status.badRequest

unauthorized :: forall a. a -> Response a
unauthorized = status Status.unauthorized

paymentRequired :: forall a. a -> Response a
paymentRequired = status Status.paymentRequired

forbidden :: forall a. a -> Response a
forbidden = status Status.forbidden

notFound :: forall a. a -> Response a
notFound = status Status.notFound

methodNotAllowed :: forall a. a -> Response a
methodNotAllowed = status Status.methodNotAllowed

notAcceptable :: forall a. a -> Response a
notAcceptable = status Status.notAcceptable

proxyAuthenticationRequired :: forall a. a -> Response a
proxyAuthenticationRequired = status Status.proxyAuthenticationRequired

requestTimeout :: forall a. a -> Response a
requestTimeout = status Status.requestTimeout

conflict :: forall a. a -> Response a
conflict = status Status.conflict

gone :: forall a. a -> Response a
gone = status Status.gone

lengthRequired :: forall a. a -> Response a
lengthRequired = status Status.lengthRequired

preconditionFailed :: forall a. a -> Response a
preconditionFailed = status Status.preconditionFailed

payloadTooLarge :: forall a. a -> Response a
payloadTooLarge = status Status.payloadTooLarge

uriTooLong :: forall a. a -> Response a
uriTooLong = status Status.uriTooLong

unsupportedMediaType :: forall a. a -> Response a
unsupportedMediaType = status Status.unsupportedMediaType

rangeNotSatisfiable :: forall a. a -> Response a
rangeNotSatisfiable = status Status.rangeNotSatisfiable

expectationFailed :: forall a. a -> Response a
expectationFailed = status Status.expectationFailed

imATeapot :: forall a. a -> Response a
imATeapot = status Status.imATeapot

misdirectedRequest :: forall a. a -> Response a
misdirectedRequest = status Status.misdirectedRequest

unprocessableEntity :: forall a. a -> Response a
unprocessableEntity = status Status.unprocessableEntity

locked :: forall a. a -> Response a
locked = status Status.locked

failedDependency :: forall a. a -> Response a
failedDependency = status Status.failedDependency

upgradeRequired :: forall a. a -> Response a
upgradeRequired = status Status.upgradeRequired

preconditionRequired :: forall a. a -> Response a
preconditionRequired = status Status.preconditionRequired

tooManyRequests :: forall a. a -> Response a
tooManyRequests = status Status.tooManyRequests

requestHeaderFieldsTooLarge :: forall a. a -> Response a
requestHeaderFieldsTooLarge = status Status.requestHeaderFieldsTooLarge

unavailableForLegalReasons :: forall a. a -> Response a
unavailableForLegalReasons = status Status.unavailableForLegalReasons

internalError :: forall a. a -> Response a
internalError = status Status.internalError

notImplemented :: forall a. a -> Response a
notImplemented = status Status.notImplemented

badGateway :: forall a. a -> Response a
badGateway = status Status.badGateway

serviceUnavailable :: forall a. a -> Response a
serviceUnavailable = status Status.serviceUnavailable

gatewayTimeout :: forall a. a -> Response a
gatewayTimeout = status Status.gatewayTimeout

httpVersionNotSupported :: forall a. a -> Response a
httpVersionNotSupported = status Status.httpVersionNotSupported

variantAlsoNegotiates :: forall a. a -> Response a
variantAlsoNegotiates = status Status.variantAlsoNegotiates

insufficientStorage :: forall a. a -> Response a
insufficientStorage = status Status.insufficientStorage

loopDetected :: forall a. a -> Response a
loopDetected = status Status.loopDetected

notExtended :: forall a. a -> Response a
notExtended = status Status.notExtended

networkAuthenticationRequired :: forall a. a -> Response a
networkAuthenticationRequired = status Status.networkAuthenticationRequired
