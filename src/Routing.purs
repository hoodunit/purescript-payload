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

class Routable apiSpec handlers guards | apiSpec -> handlers where
  mkRouter :: apiSpec -> { handlers :: handlers, guards :: guards } -> Either String (Trie HandlerEntry)

instance routableRecord ::
  ( RowToList apiSpec specList
  , RoutableList specList apiSpec (Record handlers) (Record guards)
  ) => Routable (Record apiSpec) (Record handlers) (Record guards) where
  mkRouter apiSpec { handlers, guards } =
    mkRouterList (RLProxy :: RLProxy specList) apiSpec handlers guards Trie.empty

class RoutableList (specList :: RowList) (r :: # Type) handlers guards | specList -> r where
  mkRouterList ::
    RLProxy specList
    -> Record r
    -> handlers
    -> guards
    -> Trie HandlerEntry
    -> Either String (Trie HandlerEntry)

instance routableListNil :: RoutableList Nil () handlers guards where
  mkRouterList _ _ _ _ trie = Right trie

instance routableListCons ::
  ( IsSymbol routeName
  , IsSymbol path
  , IsSymbol method
  , Row.Union spec DefaultRequest mergedSpec
  , Row.Nub mergedSpec specWithDefaults
  , Handleable (Route method path (Record specWithDefaults)) handler (Record guards)
  , RoutableList l r1 (Record handlers) (Record guards)
  , Row.Cons routeName (Route method path (Record spec)) r1 r
  , Row.Cons routeName handler h' handlers
  , ParseUrl path urlParts
  , ToSegments urlParts
  ) => RoutableList (Cons routeName (Route method path (Record spec)) l)
                    r
                    (Record handlers)
                    (Record guards) where
  mkRouterList _ rec handlers guards trie =
    case Trie.insert { route: routeSegments, handler } routeSegments trie of
      Just newTrie -> mkRouterList (RLProxy :: RLProxy l) (unsafeCoerce rec) handlers guards newTrie
      Nothing -> Left $ "Could not insert route for path '" <> reflectSymbol (SProxy :: SProxy path) <> "' into routing trie"
    where
      method = reflectSymbol (SProxy :: SProxy method)
      routeSegments = Lit method : UrlParsing.asSegments (SProxy :: SProxy path)
      payloadHandler = (get (SProxy :: SProxy routeName) handlers)
      route = Route :: Route method path (Record specWithDefaults)
      handler = handle route payloadHandler guards

class Handleable route handler guards | route -> handler where
  handle :: route -> handler -> guards -> List String -> HTTP.Request -> HTTP.Response -> Aff Outcome

instance handleablePostRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , body :: body
           , guards :: Record routeGuardSpec
           | r }
       , RowToList routeGuardSpec routeGuardList
       , RunGuards routeGuardList allGuards () routeGuardSpec
       , IsSymbol path
       , Row.Union params ( body :: body ) payload'
       , Row.Union payload' routeGuardSpec payload
       , IsRespondable res
       , PayloadUrl.DecodeUrl path params
       , FromData body
       , ParseUrl path urlParts
       , ToSegments urlParts
       )
    => Handleable (Route "POST" path (Record route)) (Record payload -> Aff res) (Record allGuards) where
  handle route handler allGuards Nil req res = do
    liftEffect $ sendError res (internalError "No path segments passed to handler")
    pure Failure
  handle route handler allGuards (method : pathSegments) req res =
    (either identity identity) <$> runExceptT (runHandler)

    where
      runHandler :: ExceptT Outcome Aff Outcome
      runHandler = do
        params <- except $ lmap (const Forward) $ decodePath pathSegments
        bodyStr <- lift $ readBody req
        body <- except $ lmap (const Forward) (Data.fromData bodyStr :: Either String body)
        let (payload' :: Record payload') = Record.union params { body }
        guards <- withExceptT (const Forward) $ ExceptT $ runGuards (RLProxy :: _ routeGuardList) allGuards {} req
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
           , guards :: Record routeGuardSpec
           | r }
       , RowToList routeGuardSpec routeGuardList
       , IsSymbol path
       , Row.Union params routeGuardSpec payload
       , IsRespondable res
       , PayloadUrl.DecodeUrl path params
       , ParseUrl path urlParts
       , ToSegments urlParts

       , RunGuards routeGuardList allGuards () routeGuardSpec
       )
    => Handleable (Route "GET" path (Record route)) (Record payload -> Aff res) (Record allGuards) where
  handle route handler allGuards Nil req res = do
    liftEffect $ sendError res (internalError "No path segments passed to handler")
    pure Failure
  handle route handler allGuards (method : pathSegments) req res =
    (either identity identity) <$> runExceptT (runHandler)

    where
      runHandler :: ExceptT Outcome Aff Outcome
      runHandler = do
        params <- except $ lmap (const Forward) $ decodePath pathSegments
        guards <- withExceptT (const Forward) $ ExceptT $ runGuards (RLProxy :: _ routeGuardList) allGuards {} req
        let (payload :: Record payload) = from (Record.union params guards)
        handlerResp <- lift $ handler payload
        liftEffect $ sendResponse res handlerResp
        pure Success

      decodePath :: List String -> Either String (Record params)
      decodePath = PayloadUrl.decodeUrl (SProxy :: SProxy path) (Proxy :: Proxy (Record params))

    -- either (const Nothing) (Just <<< handleRequest) $ do
    --   params <- decodePath pathSegments
    --   pure params
    -- where
    --   handleRequest params = Aff.launchAff_ $ Aff.catchError (runRequest params) (sendInternalError res)

    --   runRequest :: Record params -> Aff Unit
    --   runRequest params = do
    --     guardResults <- runGuards (RLProxy :: _ routeGuardList) allGuards {} req
    --     case guardResults of
    --       Right guards -> do
    --         let (payload :: Record payload) = from (Record.union params guards)
    --         handlerResp <- handler payload
    --         liftEffect $ sendResponse res handlerResp
    --       Left err -> sendInternalError res $ error $ "Guard failed" <> show err

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
