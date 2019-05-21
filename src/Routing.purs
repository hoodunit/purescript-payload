module Payload.Routing where

import Prelude

import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(Nothing, Just))
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
import Payload.GuardParsing (GuardTypes(..), Guards(..))
import Payload.Guards (class RunGuards, runGuards)
import Payload.Response (class IsRespondable, internalError, sendError, sendInternalError, sendResponse)
import Payload.Route (DefaultRequest, Route(..))
import Payload.Trie (Trie)
import Payload.Trie as Trie
import Payload.Url as PayloadUrl
import Payload.UrlParsing (class ParseUrl, class ToSegments, Segment(..))
import Payload.UrlParsing as UrlParsing
import Prim.Row as Row
import Record (get)
import Record as Record
import Type.Equality (class TypeEquals, from)
import Type.Proxy (Proxy(..))
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)
import Unsafe.Coerce (unsafeCoerce)

type HandlerEntry =
  { handler :: List String -> HTTP.Request -> HTTP.Response -> Aff Outcome
  , route :: List Segment }

data Outcome = Success | Failure | Forward

instance showOutcome :: Show Outcome where
  show Success = "Success"
  show Failure = "Failure"
  show Forward = "Forward"

class Routable routesSpec guardsSpec handlers guards | routesSpec guardsSpec -> handlers, guardsSpec -> guards where
  mkRouter :: { routes :: routesSpec, guards :: guardsSpec }
              -> { handlers :: handlers, guards :: guards }
              -> Either String (Trie HandlerEntry)

instance routableRecord ::
  ( RowToList routesSpec routesSpecList
  , RoutableList routesSpecList routesSpec guardsSpec (Record handlers) (Record guards)
  ) => Routable (Record routesSpec) (GuardTypes (Record guardsSpec)) (Record handlers) (Record guards) where
  mkRouter { routes: routesSpec, guards: guardsSpec } { handlers, guards } =
    mkRouterList (RLProxy :: RLProxy routesSpecList) routesSpec guardsSpec handlers guards Trie.empty

class RoutableList
      (routesSpecList :: RowList)
      (routesSpec :: # Type)
      (guardsSpec :: # Type)
      handlers
      guards
      | routesSpecList -> routesSpec
      , routesSpecList guardsSpec -> handlers
      , guardsSpec -> guards where
  mkRouterList ::
    RLProxy routesSpecList
    -> Record routesSpec
    -> GuardTypes (Record guardsSpec)
    -> handlers
    -> guards
    -> Trie HandlerEntry
    -> Either String (Trie HandlerEntry)

instance routableListNil :: RoutableList Nil () guardsSpec handlers guards where
  mkRouterList _ _ _ _ _ trie = Right trie

instance routableListCons ::
  ( IsSymbol routeName
  , IsSymbol path
  , IsSymbol method
  , Row.Union spec DefaultRequest mergedSpec
  , Row.Nub mergedSpec specWithDefaults
  , Handleable (Route method path (Record specWithDefaults)) handler guardsSpec (Record guards)
  , RoutableList remRoutes remRoutesSpec guardsSpec (Record handlers) (Record guards)
  , Row.Cons routeName (Route method path (Record spec)) remRoutesSpec routesSpec
  , Row.Cons routeName handler h' handlers
  , ParseUrl path urlParts
  , ToSegments urlParts
  ) => RoutableList (Cons routeName (Route method path (Record spec)) remRoutes)
                    routesSpec
                    guardsSpec
                    (Record handlers)
                    (Record guards) where
  mkRouterList _ routesSpec guardsSpec handlers guards trie =
    case Trie.insert { route: routeSegments, handler } routeSegments trie of
      Just newTrie -> mkRouterList (RLProxy :: RLProxy remRoutes)
                      (unsafeCoerce routesSpec)
                      guardsSpec
                      handlers
                      guards
                      newTrie
      Nothing -> Left $ "Could not insert route for path '" <> reflectSymbol (SProxy :: SProxy path) <> "' into routing trie"
    where
      method = reflectSymbol (SProxy :: SProxy method)
      routeSegments = Lit method : UrlParsing.asSegments (SProxy :: SProxy path)
      payloadHandler = (get (SProxy :: SProxy routeName) handlers)
      route = Route :: Route method path (Record specWithDefaults)
      guardTypes = (GuardTypes :: GuardTypes (Record guardsSpec))
      handler = handle guardTypes route payloadHandler guards

class Handleable route handler (guardsSpec :: # Type) guards | route -> handler where
  handle :: GuardTypes (Record guardsSpec) -> route -> handler -> guards -> List String -> HTTP.Request -> HTTP.Response -> Aff Outcome

instance handleablePostRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , body :: body
           , guards :: Guards guardNames
           | r }
       , RunGuards guardNames guardsSpec allGuards () routeGuardSpec
       , IsSymbol path
       , Row.Union params ( body :: body ) payload'
       , Row.Union payload' routeGuardSpec payload
       , IsRespondable res
       , PayloadUrl.DecodeUrl path params
       , FromData body
       , ParseUrl path urlParts
       , ToSegments urlParts
       )
    => Handleable (Route "POST" path (Record route)) (Record payload -> Aff res) guardsSpec (Record allGuards) where
  handle _ route handler allGuards Nil req res = do
    liftEffect $ sendError res (internalError "No path segments passed to handler")
    pure Failure
  handle _ route handler allGuards (method : pathSegments) req res =
    (either identity identity) <$> runExceptT (runHandler)

    where
      runHandler :: ExceptT Outcome Aff Outcome
      runHandler = do
        params <- except $ lmap (const Forward) $ decodePath pathSegments
        bodyStr <- lift $ readBody req
        body <- except $ lmap (const Forward) (Data.fromData bodyStr :: Either String body)
        let (payload' :: Record payload') = Record.union params { body }
        guards <- withExceptT (const Forward) $ ExceptT $
          runGuards (Guards :: _ guardNames) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
        let (payload :: Record payload) = Record.union payload' guards
        handlerResp <- lift $ handler payload
        liftEffect $ sendResponse res handlerResp
        pure Success

      decodePath :: List String -> Either String (Record params)
      decodePath = PayloadUrl.decodeUrl (SProxy :: SProxy path) (Proxy :: Proxy (Record params))

instance handleableGetRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , guards :: Guards guardNames
           | r }
       , RunGuards guardNames guardsSpec allGuards () routeGuardSpec
       , IsSymbol path
       , Row.Union params routeGuardSpec payload
       , IsRespondable res
       , PayloadUrl.DecodeUrl path params
       , ParseUrl path urlParts
       , ToSegments urlParts
       )
    => Handleable (Route "GET" path (Record route)) (Record payload -> Aff res) guardsSpec (Record allGuards) where
  handle _ route handler allGuards Nil req res = do
    liftEffect $ sendError res (internalError "No path segments passed to handler")
    pure Failure
  handle _ route handler allGuards (method : pathSegments) req res =
    (either identity identity) <$> runExceptT (runHandler)

    where
      runHandler :: ExceptT Outcome Aff Outcome
      runHandler = do
        params <- except $ lmap (const Forward) $ decodePath pathSegments
        guards <- withExceptT (const Forward) $ ExceptT $
          runGuards (Guards :: _ guardNames) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
        let (payload :: Record payload) = from (Record.union params guards)
        handlerResp <- lift $ handler payload
        liftEffect $ sendResponse res handlerResp
        pure Success

      decodePath :: List String -> Either String (Record params)
      decodePath = PayloadUrl.decodeUrl (SProxy :: SProxy path) (Proxy :: Proxy (Record params))

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
