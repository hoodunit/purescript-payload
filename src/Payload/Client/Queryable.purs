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
import Payload.Client.Options (Options, ModifyRequest)
import Payload.Client.FromResponse (class FromResponse, fromResponse)
import Payload.Client.Internal.Url as PayloadUrl
import Payload.Internal.Route (DefaultRequest)
import Payload.Response (ResponseBody(..))
import Payload.Spec (Route(Route))
import Prim.Row as Row
import Prim.Symbol as Symbol
import Record as Record
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy)

class Queryable
  route
  (basePath :: Symbol)
  (baseParams :: # Type)
  payload
  res
  | route -> payload, route -> res where
  request :: route
             -> SProxy basePath
             -> Proxy (Record baseParams)
             -> Options
             -> ModifyRequest
             -> payload
             -> Aff (Either String res)

instance queryableGetRoute ::
       ( Row.Union route DefaultRequest mergedRoute
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
       ( Row.Union route DefaultRequest mergedRoute
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
       , SimpleJson.WriteForeign body
       , SimpleJson.ReadForeign res
       )
    => Queryable (Route "POST" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts modifyReq payload = do
    let p = to payload
    let (params :: Record fullParams) = Record.delete (SProxy :: SProxy "body") p
    let url = encodeUrl opts (SProxy :: SProxy fullPath) params
    let (body :: body) = Record.get (SProxy :: SProxy "body") p
    let encodedBody = RequestBody.String (SimpleJson.writeJSON body)
    let defaultReq = AX.defaultRequest
          { method = Left POST
          , url = url
          , content = Just encodedBody
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
