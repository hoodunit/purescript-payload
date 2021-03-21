module Payload.Client.ClientApi
       ( class ClientApi
       , mkClientApi
       , class ClientApiList
       , mkClientApiList
       ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Payload.Client.Internal.Url (class EncodeUrl)
import Payload.Client.Options (Options)
import Payload.Client.Queryable (class Queryable, ClientFn, ClientFnWithOptions, request)
import Payload.Headers as Headers
import Payload.Internal.Route (DefaultParentRoute)
import Payload.Spec (Spec, Route(Route), Routes)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Record as Record
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

class ClientApi routesSpec client | routesSpec -> client where
  mkClientApi :: forall r. Options -> Spec { routes :: routesSpec | r } -> client

instance clientApiRecord ::
  ( -- Parse out child routes from root
    Row.Union rootSpec DefaultParentRoute mergedSpec
  , Row.Nub mergedSpec rootSpecWithDefaults
  , TypeEquals
      (Record rootSpecWithDefaults)
      { params :: Record rootParams
      , guards :: guards
      | childRoutes}

  -- Recurse through child routes
  , RowToList childRoutes childRoutesList
  , ClientApiList
      childRoutesList
      "" -- child base path
      rootParams -- child base params
      (Record client) -- child client
  ) => ClientApi (Record rootSpec) (Record client) where
  mkClientApi opts routesSpec = mkClientApiList
                        opts
                        (Proxy :: _ childRoutesList)
                        (Proxy :: _ "")
                        (Proxy :: _ (Record rootParams))

class ClientApiList
  (routesSpecList :: RowList Type)
  (basePath :: Symbol)
  (baseParams :: Row Type)
  client
  | routesSpecList -> client where
    mkClientApiList ::
      Options
      -> Proxy routesSpecList
      -> Proxy basePath
      -> Proxy (Record baseParams)
      -> client

instance clientApiListNil :: ClientApiList RowList.Nil basePath baseParams (Record ()) where
  mkClientApiList _ _ _ _ = {}

instance clientApiListCons ::
  ( IsSymbol routeName
  , IsSymbol routeNameWithOptions
  , IsSymbol method
  , IsSymbol path
  , Row.Cons
     routeName
     (ClientFn payload res)
     remClient
     remClient'
  , Row.Cons
     routeNameWithOptions
     (ClientFnWithOptions payload res)
     remClient'
     client
  , Symbol.Append routeName "_" routeNameWithOptions
  , Row.Lacks routeName remClient
  , Row.Lacks routeNameWithOptions remClient'
  , Queryable (Route method path routeSpec) basePath baseParams payload res
  , ClientApiList remRoutes basePath baseParams (Record remClient)
  ) => ClientApiList
         (RowList.Cons routeName (Route method path routeSpec) remRoutes)
         basePath
         baseParams
         (Record client) where
  mkClientApiList opts _ _ _ =
    Record.insert (Proxy :: _ routeName) doRequest rest
    # Record.insert (Proxy :: _ routeNameWithOptions) doRequestWithOptions
    where
      rest = mkClientApiList opts
             (Proxy :: _ remRoutes)
             (Proxy :: _ basePath)
             (Proxy :: _ (Record baseParams))
      doRequest :: ClientFn payload res
      doRequest = doRequestWithOptions { extraHeaders: Headers.empty }

      doRequestWithOptions :: ClientFnWithOptions payload res
      doRequestWithOptions reqOpts payload =
        request (Route :: Route method path routeSpec)
                (Proxy :: _ basePath)
                (Proxy :: _ (Record baseParams))
                opts
                reqOpts
                payload

instance clientApiListConsRoutes ::
  ( IsSymbol parentName
  , IsSymbol basePath
  , IsSymbol path

  -- Extra check to fail earlier and get more sensible errors for
  -- invalid parent route URL specs
  , EncodeUrl path childParams

  -- Parse out child routes from parent params
  , Row.Union parentSpec DefaultParentRoute mergedSpec
  , Row.Nub mergedSpec parentSpecWithDefaults
  , TypeEquals
      (Record parentSpecWithDefaults)
      { params :: Record parentParams
      , guards :: parentGuards
      | childRoutes}
  , Row.Union baseParams parentParams childParams

  , Row.Cons parentName (Record childClient) remClient client 

  -- Recurse through child routes
  , RowToList childRoutes childRoutesList
  , Symbol.Append basePath path childBasePath
  , ClientApiList childRoutesList childBasePath childParams (Record childClient)

  -- Iterate through rest of list of routes 
  , Row.Lacks parentName remClient
  , ClientApiList remRoutes basePath baseParams (Record remClient)
  ) => ClientApiList
         (RowList.Cons parentName (Routes path (Record parentSpec)) remRoutes)
         basePath
         baseParams
         (Record client) where
  mkClientApiList opts _ basePath baseParams =
    Record.insert
      (Proxy :: _ parentName)
      childRoutes
      (mkClientApiList opts (Proxy :: _ remRoutes) basePath baseParams)
    where
      childRoutes = mkClientApiList
                    opts
                    (Proxy :: _ childRoutesList)
                    (Proxy :: _ childBasePath)
                    (Proxy :: _ (Record childParams))
