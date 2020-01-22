module Payload.Docs.DocumentedApi where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Foreign.Object as Object
import Payload.Client.Internal.Url (class EncodeUrl)
import Payload.Internal.Route (DefaultParentRoute)
import Payload.Docs.DocumentedEndpoint (class DocumentedEndpoint, mkEndpointOpenApi)
import Payload.Docs.OpenApi (OpenApiSpec, emptyOpenApi)
import Payload.Docs.OpenApi as OpenApi
import Payload.Spec (Spec, Route(Route), Routes)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

class DocumentedApi routesSpec where
  mkOpenApiSpec :: forall r. Spec { routes :: routesSpec | r } -> OpenApiSpec

instance openApiSpecRecord ::
  ( RowToList routesSpec routesSpecList
  , DocumentedApiList routesSpecList "" ()
  ) => DocumentedApi (Record routesSpec) where
  mkOpenApiSpec routesSpec = mkOpenApiSpecList
                           (RLProxy :: _ routesSpecList)
                           (SProxy :: _ "")
                           (Proxy :: _ {})

class DocumentedApiList
  (routesSpecList :: RowList)
  (basePath :: Symbol)
  (baseParams :: # Type)
  where
    mkOpenApiSpecList ::
      RLProxy routesSpecList
      -> SProxy basePath
      -> Proxy (Record baseParams)
      -> OpenApiSpec

instance openApiSpecListNil :: DocumentedApiList RowList.Nil basePath baseParams where
  mkOpenApiSpecList _ _ _ = emptyOpenApi

instance openApiSpecListConsRoute ::
  ( IsSymbol routeName
  , IsSymbol method
  , IsSymbol path
  , IsSymbol basePath
  , DocumentedEndpoint (Route method path routeSpec) basePath baseParams payload res
  , DocumentedApiList remRoutes basePath baseParams
  ) => DocumentedApiList
         (RowList.Cons routeName (Route method path routeSpec) remRoutes)
         basePath
         baseParams where
  mkOpenApiSpecList _ _ _ = OpenApi.union endpointOpenApi rest
    where
      endpointOpenApi = mkEndpointOpenApi
                       (Route :: Route method path routeSpec)
                       (SProxy :: _ basePath)
                       (Proxy :: _ (Record baseParams))
      rest = mkOpenApiSpecList (RLProxy :: _ remRoutes)
                               (SProxy :: _ basePath)
                               (Proxy :: _ (Record baseParams))

instance openApiSpecListConsRoutes ::
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
      {params :: Record parentParams, guards :: parentGuards | childRoutes}
  , Row.Union baseParams parentParams childParams

  -- Recurse through child routes
  , RowToList childRoutes childRoutesList
  , Symbol.Append basePath path childBasePath
  , DocumentedApiList childRoutesList childBasePath childParams

  , DocumentedApiList remRoutes basePath baseParams
  ) => DocumentedApiList
         (RowList.Cons parentName (Routes path (Record parentSpec)) remRoutes)
         basePath
         baseParams where
  mkOpenApiSpecList _ _ _ = OpenApi.union childRoutes rest
    where
      childRoutes = mkOpenApiSpecList
                      (RLProxy :: _ childRoutesList)
                      (SProxy :: _ childBasePath)
                      (Proxy :: _ (Record childParams))
      rest = mkOpenApiSpecList (RLProxy :: _ remRoutes)
                               (SProxy :: _ basePath)
                               (Proxy :: _ (Record baseParams))
