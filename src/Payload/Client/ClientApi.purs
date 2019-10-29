module Payload.Client.ClientApi
       ( class ClientApi
       , mkClient
       , class ClientApiList
       , mkClientList
       ) where

import Prelude

import Data.Either (Either)
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect.Aff (Aff)
import Payload.Client.Internal.Url as PayloadUrl
import Payload.Client.Options (ModifyRequest, Options)
import Payload.Client.Queryable (class Queryable, request)
import Payload.Routable (DefaultParentRoute)
import Payload.Spec (Spec, Route(Route), Routes)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

class ClientApi routesSpec client | routesSpec -> client where
  mkClient :: forall r. Options -> Spec { routes :: routesSpec | r } -> client

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
  , Queryable (Route method path routeSpec) basePath baseParams payload res
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
