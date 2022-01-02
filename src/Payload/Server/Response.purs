-- | This module contains various helpers for returning server
-- | responses.
module Payload.Server.Response
       ( status
       , setStatus
       , updateStatus
       , setBody
       , updateBody
       , setHeaders
       , updateHeaders

       , class ToSpecResponse
       , toSpecResponse
       , class EncodeResponse
       , encodeResponse

       , continue
       , switchingProtocols
       , processing
       , ok
       , created
       , accepted
       , nonAuthoritativeInformation
       , noContent
       , resetContent
       , partialContent
       , multiStatus
       , alreadyReported
       , imUsed
       , multipleChoices
       , movedPermanently
       , found
       , seeOther
       , notModified
       , useProxy
       , temporaryRedirect
       , permanentRedirect
       , badRequest
       , unauthorized
       , paymentRequired
       , forbidden
       , notFound
       , methodNotAllowed
       , notAcceptable
       , proxyAuthenticationRequired
       , requestTimeout
       , conflict
       , gone
       , lengthRequired
       , preconditionFailed
       , payloadTooLarge
       , uriTooLong
       , unsupportedMediaType
       , rangeNotSatisfiable
       , expectationFailed
       , imATeapot
       , misdirectedRequest
       , unprocessableEntity
       , locked
       , failedDependency
       , upgradeRequired
       , preconditionRequired
       , tooManyRequests
       , requestHeaderFieldsTooLarge
       , unavailableForLegalReasons
       , internalError
       , notImplemented
       , badGateway
       , serviceUnavailable
       , gatewayTimeout
       , httpVersionNotSupported
       , variantAlsoNegotiates
       , insufficientStorage
       , loopDetected
       , notExtended
       , networkAuthenticationRequired
       ) where

import Prelude

import Control.Monad.Except (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Effect.Aff.Class (class MonadAff)
import Node.Stream as Stream
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.ResponseTypes (Empty, Failure(..), HttpStatus, Json(..), RawResponse, Response(..), ResponseBody(..), Result)
import Payload.ContentType as ContentType
import Payload.Server.Status as Status
import Payload.TypeErrors (type (<>), type (|>))
import Prim.TypeError (class Fail, Quote, Text)
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

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

-- | This type class is for converting types which are compatible with
-- | the spec into the spec type.
-- | If the spec says one type is returned from an endpoint, a handler
-- | can either return that type directly or return another type from
-- | which that type can be produced (e.g. a full response with different
-- | headers or a different status code).
class ToSpecResponse (docRoute :: Symbol) a b where
  toSpecResponse :: forall m. MonadAff m => Proxy docRoute -> a -> Result m (Response b)

instance toSpecResponseEitherFailureVal
  :: EncodeResponse a
  => ToSpecResponse docRoute (Either Failure a) a where
  toSpecResponse _ (Left err) = throwError err
  toSpecResponse _ (Right res) = pure (ok res)
else instance toSpecResponseEitherFailureResponse
  :: EncodeResponse a
  => ToSpecResponse docRoute (Either Failure (Response a)) a where
  toSpecResponse _ (Left err) = throwError err
  toSpecResponse _ (Right res) = pure res
else instance toSpecResponseEitherResponseVal
  :: EncodeResponse err
  => ToSpecResponse docRoute (Either (Response err) a) a where
  toSpecResponse _ (Left res) = do
    raw <- encodeResponse res
    throwError (Error raw) 
  toSpecResponse _ (Right res) = pure (ok res)
else instance toSpecResponseEitherResponseResponse
  :: EncodeResponse err
  => ToSpecResponse docRoute (Either (Response err) (Response a)) a where
  toSpecResponse _ (Left res) = do
    raw <- encodeResponse res
    throwError (Error raw) 
  toSpecResponse _ (Right res) = pure res
else instance toSpecResponseEitherValVal ::
  ( EncodeResponse a
  , EncodeResponse err
  ) => ToSpecResponse docRoute (Either err a) a where
  toSpecResponse _ (Left res) = do
    raw <- encodeResponse (internalError res)
    throwError (Error raw) 
  toSpecResponse _ (Right res) = pure (ok res)
else instance toSpecResponseEitherValResponse ::
  ( EncodeResponse a
  , EncodeResponse err
  ) => ToSpecResponse docRoute (Either err (Response a)) a where
  toSpecResponse _ (Left res) = do
    raw <- encodeResponse (internalError res)
    throwError (Error raw) 
  toSpecResponse _ (Right res) = pure res
else instance toSpecResponseResponse
  :: EncodeResponse a
  => ToSpecResponse docRoute (Response a) a where
  toSpecResponse _ res = pure res
else instance toSpecResponseIdentity
  :: EncodeResponse a
  => ToSpecResponse docRoute a a where
  toSpecResponse _ res = pure (ok res)
else instance toSpecResponseFail ::
  ( Fail (Text "Could not match or convert handler response type to spec response type."
          |> Text ""
          |> Text "           Route: " <> Text docRoute
          |> Text "Handler response: " <> Quote a
          |> Text "   Spec response: " <> Quote b
          |> Text ""
          |> Text "Specifically, no type class instance was found for"
          |> Text ""
          |> Text "ToSpecResponse docRoute"
          |> Text "               " <> Quote a
          |> Text "               " <> Quote b
          |> Text ""
         )
  ) => ToSpecResponse docRoute a b where
  toSpecResponse res = unsafeCoerce res

-- | Any types that can appear in a server response body and show up in the API
-- | spec under the "body" field must implement EncodeResponse. This is also
-- | a good place to add a Content-Type header for the encoded response.
class EncodeResponse r where
  encodeResponse :: forall m. MonadAff m => Response r -> Result m RawResponse
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

-- | Status code: 100
continue :: forall a. a -> Response a
continue = status Status.continue

-- | Status code: 101
switchingProtocols :: forall a. a -> Response a
switchingProtocols = status Status.switchingProtocols

-- | Status code: 102
processing :: forall a. a -> Response a
processing = status Status.processing

-- | Status code: 200
ok :: forall a. a -> Response a
ok = status Status.ok

-- | Status code: 201
created :: forall a. a -> Response a
created = status Status.created

-- | Status code: 202
accepted :: forall a. a -> Response a
accepted = status Status.accepted

-- | Status code: 203
nonAuthoritativeInformation :: forall a. a -> Response a
nonAuthoritativeInformation = status Status.nonAuthoritativeInformation

-- | Status code: 204
noContent :: forall a. a -> Response a
noContent = status Status.noContent

-- | Status code: 205
resetContent :: forall a. a -> Response a
resetContent = status Status.resetContent

-- | Status code: 206
partialContent :: forall a. a -> Response a
partialContent = status Status.partialContent

-- | Status code: 207
multiStatus :: forall a. a -> Response a
multiStatus = status Status.multiStatus

-- | Status code: 208
alreadyReported :: forall a. a -> Response a
alreadyReported = status Status.alreadyReported

-- | Status code: 226
imUsed :: forall a. a -> Response a
imUsed = status Status.imUsed

-- | Status code: 300
multipleChoices :: forall a. a -> Response a
multipleChoices = status Status.multipleChoices

-- | Status code: 301
movedPermanently :: forall a. a -> Response a
movedPermanently = status Status.movedPermanently

-- | Status code: 302
found :: forall a. a -> Response a
found = status Status.found

-- | Status code: 303
seeOther :: forall a. a -> Response a
seeOther = status Status.seeOther

-- | Status code: 304
notModified :: forall a. a -> Response a
notModified = status Status.notModified

-- | Status code: 305
useProxy :: forall a. a -> Response a
useProxy = status Status.useProxy

-- | Status code: 307
temporaryRedirect :: forall a. a -> Response a
temporaryRedirect = status Status.temporaryRedirect

-- | Status code: 308
permanentRedirect :: forall a. a -> Response a
permanentRedirect = status Status.permanentRedirect

-- | Status code: 400
badRequest :: forall a. a -> Response a
badRequest = status Status.badRequest

-- | Status code: 401
unauthorized :: forall a. a -> Response a
unauthorized = status Status.unauthorized

-- | Status code: 402
paymentRequired :: forall a. a -> Response a
paymentRequired = status Status.paymentRequired

-- | Status code: 403
forbidden :: forall a. a -> Response a
forbidden = status Status.forbidden

-- | Status code: 404
notFound :: forall a. a -> Response a
notFound = status Status.notFound

-- | Status code: 405
methodNotAllowed :: forall a. a -> Response a
methodNotAllowed = status Status.methodNotAllowed

-- | Status code: 406
notAcceptable :: forall a. a -> Response a
notAcceptable = status Status.notAcceptable

-- | Status code: 407
proxyAuthenticationRequired :: forall a. a -> Response a
proxyAuthenticationRequired = status Status.proxyAuthenticationRequired

-- | Status code: 408
requestTimeout :: forall a. a -> Response a
requestTimeout = status Status.requestTimeout

-- | Status code: 409
conflict :: forall a. a -> Response a
conflict = status Status.conflict

-- | Status code: 410
gone :: forall a. a -> Response a
gone = status Status.gone

-- | Status code: 411
lengthRequired :: forall a. a -> Response a
lengthRequired = status Status.lengthRequired

-- | Status code: 412
preconditionFailed :: forall a. a -> Response a
preconditionFailed = status Status.preconditionFailed

-- | Status code: 413
payloadTooLarge :: forall a. a -> Response a
payloadTooLarge = status Status.payloadTooLarge

-- | Status code: 414
uriTooLong :: forall a. a -> Response a
uriTooLong = status Status.uriTooLong

-- | Status code: 415
unsupportedMediaType :: forall a. a -> Response a
unsupportedMediaType = status Status.unsupportedMediaType

-- | Status code: 416
rangeNotSatisfiable :: forall a. a -> Response a
rangeNotSatisfiable = status Status.rangeNotSatisfiable

-- | Status code: 417
expectationFailed :: forall a. a -> Response a
expectationFailed = status Status.expectationFailed

-- | Status code: 418
imATeapot :: forall a. a -> Response a
imATeapot = status Status.imATeapot

-- | Status code: 421
misdirectedRequest :: forall a. a -> Response a
misdirectedRequest = status Status.misdirectedRequest

-- | Status code: 422
unprocessableEntity :: forall a. a -> Response a
unprocessableEntity = status Status.unprocessableEntity

-- | Status code: 423
locked :: forall a. a -> Response a
locked = status Status.locked

-- | Status code: 424
failedDependency :: forall a. a -> Response a
failedDependency = status Status.failedDependency

-- | Status code: 426
upgradeRequired :: forall a. a -> Response a
upgradeRequired = status Status.upgradeRequired

-- | Status code: 428
preconditionRequired :: forall a. a -> Response a
preconditionRequired = status Status.preconditionRequired

-- | Status code: 429
tooManyRequests :: forall a. a -> Response a
tooManyRequests = status Status.tooManyRequests

-- | Status code: 431
requestHeaderFieldsTooLarge :: forall a. a -> Response a
requestHeaderFieldsTooLarge = status Status.requestHeaderFieldsTooLarge

-- | Status code: 451
unavailableForLegalReasons :: forall a. a -> Response a
unavailableForLegalReasons = status Status.unavailableForLegalReasons

-- | Status code: 500
internalError :: forall a. a -> Response a
internalError = status Status.internalError

-- | Status code: 501
notImplemented :: forall a. a -> Response a
notImplemented = status Status.notImplemented

-- | Status code: 502
badGateway :: forall a. a -> Response a
badGateway = status Status.badGateway

-- | Status code: 503
serviceUnavailable :: forall a. a -> Response a
serviceUnavailable = status Status.serviceUnavailable

-- | Status code: 504
gatewayTimeout :: forall a. a -> Response a
gatewayTimeout = status Status.gatewayTimeout

-- | Status code: 505
httpVersionNotSupported :: forall a. a -> Response a
httpVersionNotSupported = status Status.httpVersionNotSupported

-- | Status code: 506
variantAlsoNegotiates :: forall a. a -> Response a
variantAlsoNegotiates = status Status.variantAlsoNegotiates

-- | Status code: 507
insufficientStorage :: forall a. a -> Response a
insufficientStorage = status Status.insufficientStorage

-- | Status code: 508
loopDetected :: forall a. a -> Response a
loopDetected = status Status.loopDetected

-- | Status code: 510
notExtended :: forall a. a -> Response a
notExtended = status Status.notExtended

-- | Status code: 511
networkAuthenticationRequired :: forall a. a -> Response a
networkAuthenticationRequired = status Status.networkAuthenticationRequired
