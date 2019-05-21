module Payload.Client where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect.Aff (Aff)
import Payload.Response (class IsRespondable, ResponseBody(..), readResponse)
import Payload.Route (DefaultRequest, Route)
import Payload.Url (class EncodeUrl)
import Payload.Url as PayloadUrl
import Prim.Row as Row
import Record as Record
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals, to)

type Options =
  { hostname :: String
  , port :: Int
  , query :: Maybe String
  }

defaultOpts :: Options
defaultOpts =
  { hostname: "localhost"
  , port: 3000
  , query: Nothing
  }

class ClientQueryable route payload res | route -> payload, route -> res where
  request :: Options -> route -> payload -> Aff (Either String res)
  request_ :: route -> payload -> Aff (Either String res)

instance clientQueryableGetRoute ::
       ( Row.Union route DefaultRequest mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           | r }
       , IsSymbol path
       , IsRespondable res
       , PayloadUrl.EncodeUrl path params
       , SimpleJson.ReadForeign res
       )
    => ClientQueryable (Route "GET" path (Record route)) (Record params) res where
  request_ route payload = request defaultOpts route payload
  request opts _ payload = do
    let params = payload
    let url = encodeUrl opts (SProxy :: SProxy path) params
    res <- AX.get ResponseFormat.string url
    pure (decodeResponse res)
else instance clientQueryablePostRoute ::
       ( Row.Union route DefaultRequest mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , body :: body
           | r }
       , TypeEquals (Record payload)
           { body :: body
           | params }
       , IsSymbol path
       , Row.Lacks "body" params
       , IsRespondable res
       , PayloadUrl.EncodeUrl path params
       , SimpleJson.WriteForeign body
       , SimpleJson.ReadForeign res
       )
    => ClientQueryable (Route "POST" path (Record route)) (Record payload) res where
  request_ route payload = request defaultOpts route payload
  request opts _ payload = do
    let p = to payload
    let (params :: Record params) = Record.delete (SProxy :: SProxy "body") p
    let url = encodeUrl opts (SProxy :: SProxy path) params
    let (body :: body) = Record.get (SProxy :: SProxy "body") p
    let encodedBody = RequestBody.String (SimpleJson.writeJSON body)
    res <- AX.post ResponseFormat.string url encodedBody
    pure (decodeResponse res)

encodeUrl :: forall path params
  . EncodeUrl path params
  => Options -> SProxy path -> Record params -> String
encodeUrl opts pathProxy params =
  "http://" <> opts.hostname <> ":" <> show opts.port <> path <> queryStr
  where
    path = PayloadUrl.encodeUrl pathProxy params
    queryStr = maybe "" (\q -> "?" <> q) opts.query

decodeResponse :: forall res. IsRespondable res =>
                 (AX.Response (Either AX.ResponseFormatError String)) -> Either String res
decodeResponse res = do
  showingError res.body >>= (StringBody >>> readResponse)
  where
    showingError = lmap ResponseFormat.printResponseFormatError
