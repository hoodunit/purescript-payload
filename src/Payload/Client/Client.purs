module Payload.Client.Client where

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
import Payload.Client.FromResponse (class ReadResponse, readResponse)
import Payload.Internal.Url (class EncodeUrl)
import Payload.Internal.Url as PayloadUrl
import Payload.Response (ResponseBody(..))
import Payload.Route (DefaultRequest)
import Payload.Routable (DefaultParentRoute)
import Payload.Spec (API, Route(Route), Routes)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Record as Record
import Simple.JSON as SimpleJson
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))

type Options =
  { hostname :: String
  , port :: Int }

type ModifyRequest = AX.Request String -> AX.Request String

defaultOpts :: Options
defaultOpts =
  { hostname: "localhost"
  , port: 3000
  }

class ClientApi routesSpec client | routesSpec -> client where
  mkClient :: forall r. Options -> API { routes :: routesSpec | r } -> client

instance clientApiRecord ::
  ( RowToList routesSpec routesSpecList
  , ClientApiList routesSpecList "" () (Record client)
  ) => ClientApi (Record routesSpec) (Record client) where
  mkClient opts routesSpec = mkClientList
                        opts
                        (RLProxy :: _ routesSpecList)
                        (SProxy :: _ "")
                        (Proxy :: _ (Record ()))

class ClientApiList
  (routesSpecList :: RowList)
  (basePath :: Symbol)
  (baseParams :: # Type)
  client
  | routesSpecList -> client where
    mkClientList ::
      Options
      -> RLProxy routesSpecList
      -> SProxy basePath
      -> Proxy (Record baseParams)
      -> client

instance clientApiListNil :: ClientApiList RowList.Nil basePath baseParams (Record ()) where
  mkClientList _ _ _ _ = {}

instance clientApiListCons ::
  ( IsSymbol routeName
  , IsSymbol method
  , IsSymbol path
  , Row.Cons
     routeName
     (ModifyRequest -> payload -> Aff (Either String res))
     remClient
     client
  , Row.Lacks routeName remClient
  , ClientQueryable (Route method path routeSpec) basePath baseParams payload res
  , ClientApiList remRoutes basePath baseParams (Record remClient)
  ) => ClientApiList
         (RowList.Cons routeName (Route method path routeSpec) remRoutes)
         basePath
         baseParams
         (Record client) where
  mkClientList opts _ _ _ =
    Record.insert
      (SProxy :: _ routeName)
      doRequest
      (mkClientList opts (RLProxy :: _ remRoutes) (SProxy :: _ basePath) (Proxy :: _ (Record baseParams)))
    where
      doRequest :: ModifyRequest -> payload -> Aff (Either String res)
      doRequest modifyReq payload =
        request (Route :: Route method path routeSpec)
                (SProxy :: _ basePath)
                (Proxy :: _ (Record baseParams))
                opts
                modifyReq
                payload

instance clientApiListConsRoutes ::
  ( IsSymbol parentName
  , IsSymbol basePath
  , IsSymbol path

  -- Parse out child routes from parent params
  , Row.Union parentSpec DefaultParentRoute mergedSpec
  , Row.Nub mergedSpec parentSpecWithDefaults
  , TypeEquals
      (Record parentSpecWithDefaults)
      {params :: Record parentParams, guards :: parentGuards | childRoutes}
  , Symbol.Append basePath path childBasePath
  , Row.Union baseParams parentParams childParams

  -- Extra check: fail here already if they don't match
  , PayloadUrl.EncodeUrl path parentParams

  -- Recurse through child routes
  , RowToList childRoutes childRoutesList
  , ClientApiList childRoutesList childBasePath childParams (Record childClient)

  -- Iterate through rest of list of routes 
  , Row.Lacks parentName remClient
  , Row.Cons parentName (Record childClient) remClient client 
  , ClientApiList remRoutes basePath baseParams (Record remClient)
  ) => ClientApiList
         (RowList.Cons parentName (Routes path (Record parentSpec)) remRoutes)
         basePath
         baseParams
         (Record client) where
  mkClientList opts _ basePath baseParams =
    Record.insert
      (SProxy :: _ parentName)
      childRoutes
      (mkClientList opts (RLProxy :: _ remRoutes) basePath baseParams)
    where
      childRoutes = mkClientList
                    opts
                    (RLProxy :: _ childRoutesList)
                    (SProxy :: _ childBasePath)
                    (Proxy :: _ (Record childParams))

class ClientQueryable
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

instance clientQueryableGetRoute ::
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
       , ReadResponse res
       , SimpleJson.ReadForeign res
       )
    => ClientQueryable (Route "GET" path (Record route)) basePath baseParams (Record fullParams) res where
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
else instance clientQueryablePostRoute ::
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
       , ReadResponse res
       , SimpleJson.WriteForeign body
       , SimpleJson.ReadForeign res
       )
    => ClientQueryable (Route "POST" path (Record route)) basePath baseParams (Record payload) res where
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
  . EncodeUrl path params
  => Options -> SProxy path -> Record params -> String
encodeUrl opts pathProxy params =
  "http://" <> opts.hostname <> ":" <> show opts.port <> path
  where
    path = PayloadUrl.encodeUrl pathProxy params

decodeResponse :: forall res. ReadResponse res =>
                 (AX.Response (Either AX.ResponseFormatError String)) -> Either String res
decodeResponse res | res.status /= StatusCode 200 = Left $ "Received HTTP " <> show res.status <> "\n" <>
  (either AX.printResponseFormatError identity res.body)
decodeResponse res = do
  showingError res.body >>= (StringBody >>> readResponse)
  where
    showingError = lmap ResponseFormat.printResponseFormatError
