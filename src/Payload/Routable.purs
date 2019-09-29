module Payload.Routable where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (errorShow, log)
import Node.HTTP as HTTP
import Payload.Handleable (class Handleable, HandlerFailure(..), HandlerM, MethodHandler, handle)
import Payload.Internal.GuardParsing (GNil, GuardTypes(..), Guards(..), kind GuardList)
import Payload.Internal.GuardParsing as GuardParsing
import Payload.Internal.Trie (Trie)
import Payload.Internal.Trie as Trie
import Payload.Internal.Url as PayloadUrl
import Payload.Internal.UrlParsing (class ParseUrl, class ToSegments, Segment(..))
import Payload.Internal.UrlParsing as UrlParsing
import Payload.Request (RequestUrl)
import Payload.Response (RawResponse(..), ServerError(..))
import Payload.Response as Resp
import Payload.Route (DefaultRequest)
import Payload.Spec (API(..), Route(Route), Routes(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Record (get)
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

type RoutingTrie = Trie HandlerEntry

type HandlerEntry =
  { handler :: RequestUrl -> HTTP.Request -> HTTP.Response -> Aff Outcome
  , route :: List Segment }

type Handler = RequestUrl -> HTTP.Request -> HTTP.Response -> Aff Outcome

data Outcome = Success | Failure | Forward String

type DefaultParentRoute = ( params :: {}, guards :: Guards GNil )
defaultParent :: Record DefaultParentRoute
defaultParent =
  { params: {}, guards: Guards :: _ GNil }

class Routable routesSpec guardsSpec handlers guards |
  routesSpec guardsSpec -> handlers,
  guardsSpec -> guards where
  mkRouter :: API { routes :: routesSpec, guards :: guardsSpec }
              -> { handlers :: handlers, guards :: guards }
              -> Either String RoutingTrie

instance routableRecord ::
  ( RowToList routesSpec routesSpecList
  , RoutableList routesSpecList "" () GNil guardsSpec (Record handlers) (Record guards)
  ) => Routable (Record routesSpec) (Record guardsSpec) (Record handlers) (Record guards) where
  mkRouter _ { handlers, guards } =
    mkRouterList
      (RLProxy :: RLProxy routesSpecList)
      (SProxy :: _ "")
      (Proxy :: _ {})
      (Guards :: _ GNil)
      (Proxy :: _ (Record guardsSpec))
      handlers
      guards
      Trie.empty

class RoutableList
      (routesSpecList :: RowList)
      (basePath :: Symbol)
      (baseParams :: # Type)
      (baseGuards :: GuardList)
      (guardsSpec :: # Type)
      handlers
      guards
      | routesSpecList guardsSpec -> handlers
      , guardsSpec -> guards where
  mkRouterList ::
    RLProxy routesSpecList
    -> SProxy basePath
    -> Proxy (Record baseParams)
    -> Guards baseGuards
    -> Proxy (Record guardsSpec)
    -> handlers
    -> guards
    -> RoutingTrie
    -> Either String RoutingTrie

instance routableListNil :: RoutableList RowList.Nil basePath baseParams baseGuards guardsSpec handlers guards where
  mkRouterList _ _ _ _ _ _ _ trie = Right trie

instance routableListCons ::
  ( IsSymbol routeName
  , IsSymbol path
  , IsSymbol method
  , Row.Union spec DefaultRequest mergedSpec
  , Row.Nub mergedSpec specWithDefaults
  , Handleable (Route method path (Record specWithDefaults)) handler basePath baseParams baseGuards guardsSpec (Record guards)
  , RoutableList remRoutes basePath baseParams baseGuards guardsSpec (Record handlers) (Record guards)
  , Row.Cons routeName handler h' handlers

  , Symbol.Append basePath path fullPath
  , ParseUrl fullPath urlParts
  , ToSegments urlParts
  ) => RoutableList (RowList.Cons routeName (Route method path (Record spec)) remRoutes)
                    basePath
                    baseParams
                    baseGuards
                    guardsSpec
                    (Record handlers)
                    (Record guards)
                    where
  mkRouterList _ basePath baseParams baseGuards guardsSpec handlers guards trie = do
    newTrie <- insertRoute (Lit method : routePath) handler trie
    trieWithRest <- mkRouterList (RLProxy :: RLProxy remRoutes)
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
      method = reflectSymbol (SProxy :: SProxy method)

      routePath :: List Segment
      routePath = UrlParsing.asSegments (SProxy :: SProxy fullPath)

      handler :: Handler
      handler url req res =
        methodHandler url req res
        # executeHandler res

      headHandler :: Handler
      headHandler url req res =
        methodHandler url req res
        <#> Resp.setEmptyBody
        # executeHandler res

      executeHandler :: HTTP.Response -> HandlerM RawResponse -> Aff Outcome
      executeHandler res mHandler = do
        result <- Aff.attempt $ runExceptT mHandler
        case result of
          Right (Right rawResponse) -> do
            liftEffect $ Resp.sendResponse res (Right rawResponse)
            pure Success
          Right (Left (HandlerError error)) -> do
            liftEffect $ Resp.sendResponse res (Left error)
            pure Failure
          Right (Left (MatchFail error)) -> pure (Forward error)
          Left error -> do
            liftEffect $ errorShow error
            liftEffect $ Resp.sendResponse res (Left (InternalError "Internal error"))
            pure Failure
      
      methodHandler :: MethodHandler
      methodHandler = handle
                      (SProxy :: _ basePath)
                      baseParams
                      baseGuards
                      (GuardTypes :: _ (Record guardsSpec))
                      (Route :: Route method path (Record specWithDefaults))
                      payloadHandler
                      guards

      payloadHandler :: handler
      payloadHandler = get (SProxy :: SProxy routeName) handlers

instance routableListConsRoutes ::
  ( IsSymbol parentName
  , IsSymbol basePath
  , IsSymbol path

  -- Parse out child routes from parent params
  , Row.Union parentSpec DefaultParentRoute mergedSpec
  , Row.Nub mergedSpec parentSpecWithDefaults
  , TypeEquals
      (Record parentSpecWithDefaults)
      {params :: Record parentParams, guards :: Guards parentGuards | childRoutes}
  , Row.Union baseParams parentParams childParams
  , GuardParsing.Append baseGuards parentGuards childGuards
  
  -- Extra check: fail here already if they don't match
  , PayloadUrl.DecodeUrl path parentParams

  , Row.Cons parentName (Record childHandlers) handlers' handlers 

  -- Recurse through child routes
  , RowToList childRoutes childRoutesList
  , Symbol.Append basePath path childBasePath
  , RoutableList childRoutesList childBasePath childParams childGuards guardsSpec (Record childHandlers) (Record guards)

  -- Iterate through rest of list routes
  , RoutableList remRoutes basePath baseParams baseGuards guardsSpec (Record handlers) (Record guards)
  ) => RoutableList (RowList.Cons parentName (Routes path (Record parentSpec)) remRoutes)
                    basePath
                    baseParams
                    baseGuards
                    guardsSpec
                    (Record handlers)
                    (Record guards) where
  mkRouterList _ basePath baseParams baseGuards guardsSpec handlers guards trie =
    case trieWithChildRoutes of
      Right newTrie -> mkRouterList (RLProxy :: RLProxy remRoutes)
                      basePath
                      baseParams
                      baseGuards
                      guardsSpec
                      handlers
                      guards
                      newTrie
      Left e -> Left $ "Could not insert child routes for path '"
                 <> reflectSymbol (SProxy :: SProxy path)
                 <> "': " <> e
    where
      childHandlers = Record.get (SProxy :: _ parentName) handlers
      trieWithChildRoutes = mkRouterList
                            (RLProxy :: _ childRoutesList)
                            (SProxy :: _ childBasePath)
                            (Proxy :: _ (Record childParams))
                            (Guards :: _ childGuards)
                            guardsSpec
                            childHandlers
                            guards
                            trie

insertRoute :: List Segment -> Handler -> RoutingTrie -> Either String RoutingTrie
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
