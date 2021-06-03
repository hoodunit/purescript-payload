module Payload.Server.Routable
       ( class Routable
       , mkRouter
       , class RoutableList
       , mkRouterList
       , HandlerEntry
       , Outcome(Success, Failure, Forward)
       ) where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List, (:))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (errorShow)
import Effect.Exception (Error)
import Node.HTTP as HTTP
import Payload.Internal.Route (DefaultParentRoute, DefaultServerRouteSpec)
import Payload.Internal.UrlParsing (class ParseUrl, class ToSegments, Segment(..))
import Payload.Internal.UrlParsing as UrlParsing
import Payload.ResponseTypes (Failure(..)) as Resp
import Payload.ResponseTypes (RawResponse, ResponseBody(..), Result)
import Payload.Server.Handleable (class Handleable, MethodHandler, handle)
import Payload.Server.Internal.GuardParsing (GuardTypes(GuardTypes))
import Payload.Server.Internal.GuardParsing as GuardParsing
import Payload.Server.Internal.Request (RequestUrl)
import Payload.Server.Internal.ServerResponse (sendResponse)
import Payload.Server.Internal.Trie (Trie)
import Payload.Server.Internal.Trie as Trie
import Payload.Server.Internal.Url as PayloadUrl
import Payload.Server.Response (internalError, setBody) as Resp
import Payload.Spec (GuardList, Spec, Guards(Guards), Route(Route), Routes)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Record (get)
import Record as Record
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

type RoutingTrie m = Trie (HandlerEntry m)

type HandlerEntry m =
  { handler :: RequestUrl -> HTTP.Request -> HTTP.Response -> m Outcome
  , route :: List Segment }

type RawHandler m = RequestUrl -> HTTP.Request -> HTTP.Response -> m Outcome

data Outcome = Success | Failure | Forward String

class Routable routesSpec guardsSpec handlers guards m |
  routesSpec guardsSpec -> handlers,
  guardsSpec -> guards where
  mkRouter :: Spec { routes :: routesSpec, guards :: guardsSpec }
              -> { handlers :: handlers, guards :: guards }
              -> Either String (RoutingTrie m)

instance routableRootRecord ::
  (
  -- Parse out child routes from root
    Row.Union rootSpec DefaultParentRoute mergedSpec
  , Row.Nub mergedSpec rootSpecWithDefaults
  , TypeEquals
      (Record rootSpecWithDefaults)
      { params :: Record rootParams
      , guards :: Guards rootGuards
      | childRoutes}

  -- Recurse through child routes
  , RowToList childRoutes childRoutesList
  , RoutableList
      childRoutesList
      "" -- child base path
      rootParams -- child base params
      rootGuards -- child base guards
      guardsSpec
      handlers
      guards
      m
  ) => Routable
         (Record rootSpec)
         (Record guardsSpec)
         handlers
         guards
         m where
  mkRouter _ { handlers, guards } =
    mkRouterList
      (Proxy :: _ childRoutesList)
      (Proxy :: _ "")
      (Proxy :: _ (Record rootParams))
      (Guards :: _ rootGuards)
      (Proxy :: _ (Record guardsSpec))
      handlers
      guards
      Trie.empty

class RoutableList
      (routesSpecList :: RowList Type)
      (basePath :: Symbol)
      (baseParams :: Row Type)
      (baseGuards :: GuardList)
      (guardsSpec :: Row Type)
      handlers
      guards
      m
      | routesSpecList guardsSpec -> handlers
      , guardsSpec -> guards where
  mkRouterList ::
    Proxy routesSpecList
    -> Proxy basePath
    -> Proxy (Record baseParams)
    -> Guards baseGuards
    -> Proxy (Record guardsSpec)
    -> handlers
    -> guards
    -> RoutingTrie m
    -> Either String (RoutingTrie m)

instance routableListNil :: RoutableList RL.Nil basePath baseParams baseGuards guardsSpec handlers guards m where
  mkRouterList _ _ _ _ _ _ _ trie = Right trie

instance routableListCons ::
  ( IsSymbol routeName
  , IsSymbol path
  , IsSymbol method
  , Row.Union spec DefaultServerRouteSpec mergedSpec
  , Row.Nub mergedSpec specWithDefaults
  , Handleable (Route method path (Record specWithDefaults)) handler basePath baseParams baseGuards guardsSpec (Record guards) m
  , RoutableList remRoutes basePath baseParams baseGuards guardsSpec (Record handlers) (Record guards) m
  , Row.Cons routeName handler h' handlers

  , Symbol.Append basePath path fullPath
  , ParseUrl fullPath urlParts
  , ToSegments urlParts
  , MonadEffect m
  ) => RoutableList (RL.Cons routeName (Route method path (Record spec)) remRoutes)
                    basePath
                    baseParams
                    baseGuards
                    guardsSpec
                    (Record handlers)
                    (Record guards)
                    m
                    where
  mkRouterList _ basePath baseParams baseGuards guardsSpec handlers guards trie = do
    newTrie <- insertRoute (Lit method : routePath) handler trie
    trieWithRest <- mkRouterList (Proxy :: Proxy remRoutes)
          basePath
          baseParams
          baseGuards
          guardsSpec
          handlers
          guards
          newTrie
    case method of
      "GET" -> orElse (const trieWithRest) $ insertRoute (Lit "HEAD" : routePath) headHandler trieWithRest
      _ -> pure trieWithRest
    where
      method :: String
      method = reflectSymbol (Proxy :: Proxy method)

      routePath :: List Segment
      routePath = UrlParsing.asSegments (Proxy :: Proxy fullPath)

      handler :: RawHandler m
      handler url req res =
        methodHandler url req res
        # executeHandler res

      headHandler :: RawHandler m
      headHandler url req res =
        methodHandler url req res
        <#> Resp.setBody EmptyBody
        # executeHandler res
      
      methodHandler :: MethodHandler _
      methodHandler = handle
                      (Proxy :: _ basePath)
                      baseParams
                      baseGuards
                      (GuardTypes :: _ (Record guardsSpec))
                      (Route :: Route method path (Record specWithDefaults))
                      payloadHandler
                      guards

      payloadHandler :: handler
      payloadHandler = get (Proxy :: Proxy routeName) handlers

executeHandler :: forall m. MonadEffect m => HTTP.Response -> Result m RawResponse -> m Outcome
executeHandler res mHandler = do
  result <- runExceptT mHandler
  case result of
    Right rawResponse -> do
      liftEffect $ sendResponse res rawResponse
      pure Success
    Left (Resp.Error errorResp) -> do
      liftEffect $ sendResponse res errorResp
      pure Failure
    Left (Resp.Forward error) -> pure (Forward error)

instance routableListConsRoutes ::
  ( IsSymbol parentName
  , IsSymbol basePath
  , IsSymbol path
  
  -- Extra check to fail earlier and get more sensible errors for
  -- invalid parent route URL specs
  , PayloadUrl.DecodeUrl path parentParams

  -- Parse out child routes from parent params
  , Row.Union parentSpec DefaultParentRoute mergedSpec
  , Row.Nub mergedSpec parentSpecWithDefaults
  , TypeEquals
      (Record parentSpecWithDefaults)
      { params :: Record parentParams
      , guards :: Guards parentGuards
      | childRoutes}
  , Row.Union baseParams parentParams childParams
  , GuardParsing.Append baseGuards parentGuards childGuards

  , Row.Cons parentName (Record childHandlers) handlers' handlers 

  -- Recurse through child routes
  , RowToList childRoutes childRoutesList
  , Symbol.Append basePath path childBasePath
  , RoutableList childRoutesList childBasePath childParams childGuards guardsSpec (Record childHandlers) (Record guards) m

  -- Iterate through rest of list routes
  , RoutableList remRoutes basePath baseParams baseGuards guardsSpec (Record handlers) (Record guards) m
  ) => RoutableList (RL.Cons parentName (Routes path (Record parentSpec)) remRoutes)
                    basePath
                    baseParams
                    baseGuards
                    guardsSpec
                    (Record handlers)
                    (Record guards)
                    m where
  mkRouterList _ basePath baseParams baseGuards guardsSpec handlers guards trie =
    case trieWithChildRoutes of
      Right newTrie -> mkRouterList (Proxy :: Proxy remRoutes)
                      basePath
                      baseParams
                      baseGuards
                      guardsSpec
                      handlers
                      guards
                      newTrie
      Left e -> Left $ "Could not insert child routes for path '"
                 <> reflectSymbol (Proxy :: Proxy path)
                 <> "': " <> e
    where
      childHandlers = Record.get (Proxy :: _ parentName) handlers
      trieWithChildRoutes = mkRouterList
                            (Proxy :: _ childRoutesList)
                            (Proxy :: _ childBasePath)
                            (Proxy :: _ (Record childParams))
                            (Guards :: _ childGuards)
                            guardsSpec
                            childHandlers
                            guards
                            trie

insertRoute :: forall m. List Segment -> RawHandler m -> RoutingTrie m -> Either String (RoutingTrie m)
insertRoute route handler trie = lmap wrapError $ Trie.insert {route, handler} route trie
  where
    handlerEntry = { route, handler }

    wrapError :: String -> String
    wrapError e = "Could not insert route for path '" <>
                  show route <>
                  "' into routing trie"

orElse :: forall a b c. (a -> c) -> Either a c -> Either b c
orElse _ (Right v) = Right v 
orElse f (Left v) = Right (f v) 
