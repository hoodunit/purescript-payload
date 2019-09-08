module Payload.Routing where

import Prelude

import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Exception as Ex
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.HTTP as HTTP
import Node.Stream (onDataString, onEnd, onError)
import Payload.Data (class FromData)
import Payload.Data as Data
import Payload.GuardParsing (GNil, GuardTypes(..), Guards(..), kind GuardList)
import Payload.GuardParsing as GuardParsing
import Payload.Guards (class RunGuards, runGuards)
import Payload.Query (decodeQuery)
import Payload.Query as PayloadQuery
import Payload.Request (RequestUrl)
import Payload.Response (class Responder, internalError, sendError, sendInternalError, sendResponse)
import Payload.Route (DefaultRequest, Route(..))
import Payload.Trie (Trie)
import Payload.Trie as Trie
import Payload.Url as PayloadUrl
import Payload.UrlParsing (class ParseUrl, class ToSegments, Segment(..), UrlNil)
import Payload.UrlParsing as UrlParsing
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Record (get)
import Record as Record
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals, from)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type HandlerEntry =
  { handler :: RequestUrl -> HTTP.Request -> HTTP.Response -> Aff Outcome
  , route :: List Segment }

data Outcome = Success | Failure | Forward String

data API apiSpec = API
data Routes (path :: Symbol) routesSpec = Routes

type DefaultParentRoute = ( params :: {}, guards :: Guards GNil )
defaultParent :: Record DefaultParentRoute
defaultParent =
  { params: {}, guards: Guards :: _ GNil }

class Routable routesSpec guardsSpec handlers guards |
  routesSpec guardsSpec -> handlers,
  guardsSpec -> guards where
  mkRouter :: API { routes :: routesSpec, guards :: guardsSpec }
              -> { handlers :: handlers, guards :: guards }
              -> Either String (Trie HandlerEntry)

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
    -> Trie HandlerEntry
    -> Either String (Trie HandlerEntry)

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
    newTrie <- lmap wrapError $ Trie.insert { route: routeSegments, handler } routeSegments trie
    mkRouterList (RLProxy :: RLProxy remRoutes) basePath baseParams baseGuards guardsSpec handlers guards newTrie
    where
      method = reflectSymbol (SProxy :: SProxy method)
      routeSegments = (Lit method : Nil) <> UrlParsing.asSegments (SProxy :: SProxy fullPath)
      payloadHandler = (get (SProxy :: SProxy routeName) handlers)
      route = Route :: Route method path (Record specWithDefaults)
      guardTypes = (GuardTypes :: GuardTypes (Record guardsSpec))
      handler = handle (SProxy :: _ basePath) baseParams baseGuards guardTypes route payloadHandler guards

      wrapError :: String -> String
      wrapError e = "Could not insert route for path '" <>
                    reflectSymbol (SProxy :: SProxy path) <>
                    "' into routing trie:\n" <>
                    "  Full path: " <> show routeSegments <> "\n" <> e

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

class Handleable
  route
  handler
  (basePath :: Symbol)
  (baseParams :: # Type)
  (baseGuards :: GuardList)
  (guardsSpec :: # Type)
  guards | route -> handler where
  handle :: SProxy basePath
            -> Proxy (Record baseParams)
            -> Guards baseGuards
            -> GuardTypes (Record guardsSpec)
            -> route
            -> handler
            -> guards
            -> RequestUrl
            -> HTTP.Request
            -> HTTP.Response
            -> Aff Outcome

instance handleablePostRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , body :: body
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Responder res
       , Symbol.Append basePath path fullPath
       , FromData body

       , Row.Union baseParams params fullUrlParams
       , Row.Union fullUrlParams query fullParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , Row.Union fullParams ( body :: body ) payload'
       , Row.Union payload' routeGuardSpec payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec
       )
    => Handleable (Route "POST" path (Record route))
                  (Record payload -> Aff res)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards) where
  handle _ _ _ _ route handler allGuards { method, path, query } req res =
    (either identity identity) <$> runExceptT runHandler

    where
      runHandler :: ExceptT Outcome Aff Outcome
      runHandler = do
        params <- except $ lmap Forward $ decodePath path
        decodedQuery <- except $ lmap Forward $ decodeQuery_ query
        bodyStr <- lift $ readBody req
        body <- except $ lmap Forward (Data.fromData bodyStr :: Either String body)
        let (fullParams :: Record fullParams) = from (Record.union params decodedQuery)
        let (payload' :: Record payload') = Record.union fullParams { body }
        guards <- withExceptT Forward $ ExceptT $
          runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
        let (payload :: Record payload) = Record.union payload' guards
        handlerResp <- lift $ handler payload
        liftEffect $ sendResponse res handlerResp
        pure Success

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (SProxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery_ :: String -> Either String (Record query)
      decodeQuery_ = PayloadQuery.decodeQuery (SProxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleableGetRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Responder res
       , Symbol.Append basePath path fullPath

       , Row.Union baseParams params fullUrlParams
       , Row.Union fullUrlParams query fullParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , Row.Union fullParams routeGuardSpec payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec
       )
    => Handleable (Route "GET" path (Record route))
                  (Record payload -> Aff res)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards) where
  handle _ _ _ _ route handler allGuards { method, path, query } req res =
    (either identity identity) <$> runExceptT runHandler

    where
      runHandler :: ExceptT Outcome Aff Outcome
      runHandler = do
        params <- except $ lmap Forward $ decodePath path
        decodedQuery <- except $ lmap Forward $ decodeQuery_ query
        guards <- withExceptT Forward $ ExceptT $
          runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
        let (fullParams :: Record fullParams) = from (Record.union params decodedQuery)
        let (payload :: Record payload) = from (Record.union fullParams guards)
        handlerResp <- lift $ handler payload
        liftEffect $ sendResponse res handlerResp
        pure Success

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (SProxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery_ :: String -> Either String (Record query)
      decodeQuery_ = PayloadQuery.decodeQuery (SProxy :: _ fullPath) (Proxy :: _ (Record query))

readBody :: HTTP.Request -> Aff String
readBody req = Aff.makeAff (readBody_ req)

readBody_ :: HTTP.Request -> (Either Error String -> Effect Unit) -> Effect Aff.Canceler
readBody_ req cb = do
  buffer <- Ref.new ""
  let inputStream = HTTP.requestAsStream req
  let handleData str = (flip Ref.modify_) buffer (_ <> str)
  let handleEnd = Ref.read buffer >>= returnBody
  Ex.catchException returnError do
    onError inputStream returnError
    onDataString inputStream UTF8 handleData
    onEnd inputStream handleEnd
  pure mempty
  where
    returnError msg = cb $ Left $ msg
    returnBody val = cb $ Right $ val
