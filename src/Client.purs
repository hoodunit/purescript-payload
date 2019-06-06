module Payload.Client where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect.Aff (Aff)
import Payload.Response (class IsRespondable, ResponseBody(..), readResponse)
import Payload.Route (DefaultRequest, Route(..))
import Payload.Routing (API(..), DefaultParentRoute, Routes(..))
import Payload.Url (class EncodeUrl)
import Payload.Url as PayloadUrl
import Prim.Row (class Cons)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Text)
import Record as Record
import Simple.JSON as SimpleJson
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

class ClientApi routesSpec client | routesSpec -> client where
  mkClient :: forall r. API { routes :: routesSpec | r } -> client

instance clientApiRecord ::
  ( RowToList routesSpec routesSpecList
  , ClientApiList routesSpecList "" () (Record client)
  ) => ClientApi (Record routesSpec) (Record client) where
  mkClient routesSpec = mkClientList
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
      RLProxy routesSpecList
      -> SProxy basePath
      -> Proxy (Record baseParams)
      -> client

instance clientApiListNil :: ClientApiList RowList.Nil basePath baseParams (Record ()) where
  mkClientList _ _ _ = {}

instance clientApiListCons ::
  ( IsSymbol routeName
  , IsSymbol method
  , IsSymbol path
  , Row.Cons
     routeName
     (Options -> payload -> Aff (Either String res))
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
  mkClientList _ _ _ =
    Record.insert
      (SProxy :: _ routeName)
      doRequest
      (mkClientList (RLProxy :: _ remRoutes) (SProxy :: _ basePath) (Proxy :: _ (Record baseParams)))
    where
      doRequest :: Options -> payload -> Aff (Either String res)
      doRequest opt payload =
        request (Route :: Route method path routeSpec)
                (SProxy :: _ basePath)
                (Proxy :: _ (Record baseParams))
                opt
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
  mkClientList _ basePath baseParams =
    Record.insert
      (SProxy :: _ parentName)
      childRoutes
      (mkClientList (RLProxy :: _ remRoutes) basePath baseParams)
    where
      childRoutes = mkClientList
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
  request :: route -> SProxy basePath -> Proxy (Record baseParams) -> Options -> payload -> Aff (Either String res)

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
       , IsRespondable res
       , SimpleJson.ReadForeign res
       )
    => ClientQueryable (Route "GET" path (Record route)) basePath baseParams (Record fullParams) res where
  request _ _ _ opts payload = do
    let params = payload
    let url = encodeUrl opts (SProxy :: _ fullPath) params
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
       , Row.Union baseParams params fullParams
       , TypeEquals (Record payload)
           { body :: body
           | fullParams }
       , Row.Lacks "body" fullParams
       , IsSymbol path
       , Symbol.Append basePath path fullPath
       , PayloadUrl.EncodeUrl fullPath fullParams
       , IsRespondable res
       , SimpleJson.WriteForeign body
       , SimpleJson.ReadForeign res
       )
    => ClientQueryable (Route "POST" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts payload = do
    let p = to payload
    let (params :: Record fullParams) = Record.delete (SProxy :: SProxy "body") p
    let url = encodeUrl opts (SProxy :: SProxy fullPath) params
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
decodeResponse res | res.status /= StatusCode 200 = Left $ "Received HTTP " <> show res.status <> "\n" <>
  (either AX.printResponseFormatError identity res.body)
decodeResponse res = do
  showingError res.body >>= (StringBody >>> readResponse)
  where
    showingError = lmap ResponseFormat.printResponseFormatError
