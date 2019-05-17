module Payload.Client where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Aff (Aff)
import Payload.Response (class IsRespondable, ResponseBody(..), readResponse)
import Payload.Routing (DefaultRequest, Route(..))
import Payload.Url as PayloadUrl
import Prim.Row as Row
import Record as Record
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals, to)

class ClientQueryable route payload res | route -> payload, route -> res where
  request :: route -> payload -> Aff (Either String res)

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
  request _ payload = do
    let params = payload
    let path = PayloadUrl.encodeUrl (SProxy :: SProxy path) params
    let url = "http://localhost:3000" <> path
    doGet url
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
  request _ payload = do
    let p = to payload
    let (params :: Record params) = Record.delete (SProxy :: SProxy "body") p
    let path = PayloadUrl.encodeUrl (SProxy :: SProxy path) params
    let url = "http://localhost:3000" <> path
    let (body :: body) = Record.get (SProxy :: SProxy "body") p
    doPost url body

doGet :: forall res. IsRespondable res => String -> Aff (Either String res)
doGet url = do
  res <- AX.get ResponseFormat.string url
  let showingError = lmap ResponseFormat.printResponseFormatError
  pure $ showingError res.body >>= (StringBody >>> readResponse
)

doPost :: forall res body
          . SimpleJson.WriteForeign body
          => IsRespondable res
          => String -> body -> Aff (Either String res)
doPost url body = do
  let encodedBody = RequestBody.String (SimpleJson.writeJSON body)
  res <- AX.post ResponseFormat.string url encodedBody
  let showingError = lmap ResponseFormat.printResponseFormatError
  pure $ showingError res.body >>= (StringBody >>> readResponse)
