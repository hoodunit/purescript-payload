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
import Type.Equality (class TypeEquals, from)

type DefaultReqParams = ( params :: {}, body :: {} )
defaultReqParams :: Record DefaultReqParams
defaultReqParams =
  { params: {}
  , body: {}
  }

class ClientQueryable route reqParams res | route -> reqParams, route -> res where
  request :: route -> Record reqParams -> Aff (Either String res)

instance clientQueryableRoute ::
       ( Row.Union route DefaultRequest mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , Row.Union reqParams DefaultReqParams mergedReqParams
       , Row.Nub mergedReqParams reqParamsWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , body :: body
           | r }
       , IsSymbol path
       , IsSymbol method
       , TypeEquals { params :: Record params, body :: body } (Record reqParamsWithDefaults)
       , IsRespondable res
       , PayloadUrl.EncodeUrl path params
       , SimpleJson.WriteForeign body
       , SimpleJson.ReadForeign res
       )
    => ClientQueryable (Route method path (Record route)) reqParams res where
  request route reqParams = do
    let method = reflectSymbol (SProxy :: SProxy method)
    let fullReqParams = Record.merge reqParams defaultReqParams
    let params = (from fullReqParams).params
    let path = PayloadUrl.encodeUrl (SProxy :: SProxy path) params
    let url = "http://localhost:3000" <> path
    if method == "GET"
      then doGet url
      else do
        let body = (from fullReqParams).body
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
