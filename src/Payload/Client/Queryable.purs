module Payload.Client.Queryable where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect.Aff (Aff)
import Payload.Client.FromResponse (class FromResponse, fromResponse)
import Payload.Client.Internal.Url as PayloadUrl
import Payload.Client.Options (Options, ModifyRequest)
import Payload.Internal.Route (DefaultRouteSpec, DefaultRouteSpecNoBody, NoBody)
import Payload.Response (ResponseBody(..))
import Payload.Spec (Route)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Record as Record
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals, from, to)
import Type.Proxy (Proxy(..))

class Queryable
  route
  (basePath :: Symbol)
  (baseParams :: # Type)
  payload
  res
  | route baseParams basePath -> payload, route -> res where
  request :: route
             -> SProxy basePath
             -> Proxy (Record baseParams)
             -> Options
             -> ModifyRequest
             -> payload
             -> Aff (Either String res)

instance queryableGetRoute ::
       ( Row.Lacks "body" route
       , Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           | r }
       , IsSymbol path
       , Symbol.Append basePath path fullPath
       , PayloadUrl.EncodeUrl fullPath fullParams
       , Row.Union baseParams params fullParams
       , FromResponse res
       , SimpleJson.ReadForeign res
       )
    => Queryable (Route "GET" path (Record route)) basePath baseParams (Record fullParams) res where
  request _ _ _ opts modifyReq payload = do
    let params = payload
    let url = encodeUrl opts (SProxy :: _ fullPath) params
    let defaultReq = AX.defaultRequest
          { method = Left GET
          , url = url
          , responseFormat = ResponseFormat.string }
    let req = modifyReq defaultReq
    res <- AX.request req
    pure (decodeResponse res)
else instance queryablePostRoute ::
       ( Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , body :: body
           | r }
       , Row.Union baseParams params fullParams
       , TypeEquals (Record payload)
           { body :: body
           | fullParams }
       , Row.Lacks "body" fullParams
       , IsSymbol path
       , Symbol.Append basePath path fullPath
       , PayloadUrl.EncodeUrl fullPath fullParams
       , FromResponse res
       , EncodeBody body
       , SimpleJson.ReadForeign res
       )
    => Queryable (Route "POST" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts modifyReq payload = do
    let p = to payload
    let (params :: Record fullParams) = Record.delete (SProxy :: SProxy "body") p
    let url = encodeUrl opts (SProxy :: SProxy fullPath) params
    let (body :: body) = Record.get (SProxy :: SProxy "body") p
    let encodedBody = RequestBody.String (encodeBody body)
    let defaultReq = AX.defaultRequest
          { method = Left POST
          , url = url
          , content = Just encodedBody
          , responseFormat = ResponseFormat.string }
    let req = modifyReq defaultReq
    res <- AX.request req
    pure (decodeResponse res)
else instance queryableHeadRoute ::
       ( Row.Lacks "body" route
       , Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { params :: Record params
           | r }
       , IsSymbol path
       , Symbol.Append basePath path fullPath
       , PayloadUrl.EncodeUrl fullPath fullParams
       , Row.Union baseParams params fullParams
       )
    => Queryable (Route "HEAD" path (Record route)) basePath baseParams (Record fullParams) String where
  request _ _ _ opts modifyReq payload = do
    let params = payload
    let url = encodeUrl opts (SProxy :: _ fullPath) params
    let defaultReq = AX.defaultRequest
          { method = Left HEAD
          , url = url
          , responseFormat = ResponseFormat.string }
    let req = modifyReq defaultReq
    res <- AX.request req
    pure (decodeResponse res)
else instance queryablePutRoute ::
       ( Row.Union route DefaultRouteSpecNoBody mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , body :: body
           | r }
       , Row.Union baseParams params fullParams
       , IsSymbol path
       , Symbol.Append basePath path fullPath
       , PayloadUrl.EncodeUrl fullPath fullParams
       , Row.Lacks "body" fullParams
       , EncodeOptionalBody body fullParams payload
       , FromResponse res
       , SimpleJson.ReadForeign res
       )
    => Queryable (Route "PUT" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts modifyReq payload = do
    let { body, fullParams } = encodeOptionalBody (Proxy :: _ body) (Proxy :: _ (Record fullParams)) payload
    let url = encodeUrl opts (SProxy :: SProxy fullPath) fullParams
    let defaultReq = AX.defaultRequest
          { method = Left PUT
          , url = url
          , content = body
          , responseFormat = ResponseFormat.string }
    let req = modifyReq defaultReq
    res <- AX.request req
    pure (decodeResponse res)
else instance queryableDeleteRoute ::
       ( Row.Union route DefaultRouteSpecNoBody mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , body :: body
           | r }
       , Row.Union baseParams params fullParams
       , IsSymbol path
       , Symbol.Append basePath path fullPath
       , PayloadUrl.EncodeUrl fullPath fullParams
       , Row.Lacks "body" fullParams
       , EncodeOptionalBody body fullParams payload
       , FromResponse res
       , SimpleJson.ReadForeign res
       )
    => Queryable (Route "DELETE" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts modifyReq payload = do
    let { body, fullParams } = encodeOptionalBody (Proxy :: _ body) (Proxy :: _ (Record fullParams)) payload
    let url = encodeUrl opts (SProxy :: SProxy fullPath) fullParams
    let defaultReq = AX.defaultRequest
          { method = Left DELETE
          , url = url
          , content = body
          , responseFormat = ResponseFormat.string }
    let req = modifyReq defaultReq
    res <- AX.request req
    pure (decodeResponse res)

encodeUrl :: forall path params
  . PayloadUrl.EncodeUrl path params
  => Options -> SProxy path -> Record params -> String
encodeUrl opts pathProxy params =
  "http://" <> opts.hostname <> ":" <> show opts.port <> path
  where
    path = PayloadUrl.encodeUrl pathProxy params

decodeResponse :: forall res. FromResponse res =>
                 (AX.Response (Either AX.ResponseFormatError String)) -> Either String res
decodeResponse res | res.status /= StatusCode 200 = Left $ "Received HTTP " <> show res.status <> "\n" <>
  (either AX.printResponseFormatError identity res.body)
decodeResponse res = do
  showingError res.body >>= (StringBody >>> fromResponse)
  where
    showingError = lmap ResponseFormat.printResponseFormatError

-- | Allows specs to optionally specify a body for HTTP methods where
-- | a body may or may not appear. If a body is specified in the spec,
-- | it is required in client requests (by deriving the type here as
-- | including the body).
class EncodeOptionalBody
  (body :: Type)
  (fullParams :: # Type)
  (payload :: # Type)
  | body fullParams -> payload
  where
    encodeOptionalBody :: Proxy body
                  -> Proxy (Record fullParams)
                  -> Record payload
                  -> { body :: Maybe RequestBody.RequestBody, fullParams :: Record fullParams }

instance encodeOptionalBodyNoBody :: EncodeOptionalBody NoBody fullParams fullParams where
  encodeOptionalBody _ _ payload = { body: Nothing, fullParams: payload }
else instance encodeOptionalBodyWithBody ::
  ( Row.Lacks "body" fullParams
  , EncodeBody body
  ) => EncodeOptionalBody body fullParams (body :: body | fullParams) where
  encodeOptionalBody _ _ payload = { body, fullParams }
    where
      body = Just $ RequestBody.String $ encodeBody payload.body
      fullParams = Record.delete (SProxy :: _ "body") payload

class EncodeBody body where
  encodeBody :: body -> String

instance encodeBodyString :: EncodeBody String where
  encodeBody b = b

instance encodeBodyRecord :: SimpleJson.WriteForeign (Record r) => EncodeBody (Record r) where
  encodeBody = SimpleJson.writeJSON

instance encodeBodyArray :: SimpleJson.WriteForeign (Array r) => EncodeBody (Array r) where
  encodeBody = SimpleJson.writeJSON
