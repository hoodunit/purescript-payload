module Payload.Routing where

import Prelude

import Data.Either (Either(..))
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
import Payload.Response (class IsRespondable, internalError, sendError, sendInternalError, sendResponse)
import Payload.Trie (Trie)
import Payload.Trie as Trie
import Payload.Url as PayloadUrl
import Payload.UrlParsing (class ParseUrl, class ToSegments, Segment(..))
import Payload.UrlParsing as UrlParsing
import Prim.Row as Row
import Record (get)
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals, from)
import Type.Proxy (Proxy(..))
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)
import Unsafe.Coerce (unsafeCoerce)

data Route (m :: Symbol) (p :: Symbol) spec = Route
type GET = Route "GET"
type POST = Route "POST"

type DefaultRequest = ( params :: {}, body :: {} )
defaultSpec :: Record DefaultRequest
defaultSpec =
  { params: {}
  , body: {}
  }

type Handler = List String -> HTTP.Request -> HTTP.Response -> Maybe (Effect Unit)
type HandlerEntry =
  { handler :: List String -> HTTP.Request -> HTTP.Response -> Maybe (Effect Unit)
  , route :: List Segment }

class Routable apiSpec handlers where
  mkRouter :: apiSpec -> handlers -> Either String (Trie HandlerEntry)

instance routableRecord ::
  ( RowToList apiSpec specList
  , RoutableList specList apiSpec (Record handlers)
  ) => Routable (Record apiSpec) (Record handlers) where
  mkRouter apiSpec handlers = mkRouterList (RLProxy :: RLProxy specList) apiSpec handlers Trie.empty

class RoutableList (specList :: RowList) (r :: # Type) handlers | specList -> r where
  mkRouterList ::
    RLProxy specList
    -> Record r
    -> handlers
    -> Trie HandlerEntry
    -> Either String (Trie HandlerEntry)

instance routableListNil :: RoutableList Nil () h where
  mkRouterList _ _ _ trie = Right trie

instance routableListCons ::
  ( IsSymbol routeName
  , IsSymbol path
  , IsSymbol method
  , Row.Union spec DefaultRequest mergedSpec
  , Row.Nub mergedSpec specWithDefaults
  , Handleable (Route method path (Record specWithDefaults)) handler
  , RoutableList l r1 (Record handlers)
  , Row.Cons routeName (Route method path (Record spec)) r1 r
  , Row.Cons routeName handler h' handlers
  , ParseUrl path urlParts
  , ToSegments urlParts
  ) => RoutableList (Cons routeName (Route method path (Record spec)) l) r (Record handlers) where
  mkRouterList _ rec handlers trie =
    case Trie.insert { route: routeSegments, handler } routeSegments trie of
      Just newTrie -> mkRouterList (RLProxy :: RLProxy l) (unsafeCoerce rec) handlers newTrie
      Nothing -> Left $ "Could not insert route for path '" <> reflectSymbol (SProxy :: SProxy path) <> "' into routing trie"
    where
      method = reflectSymbol (SProxy :: SProxy method)
      routeSegments = Lit method : UrlParsing.asSegments (SProxy :: SProxy path)
      payloadHandler = (get (SProxy :: SProxy routeName) handlers)
      route = Route :: Route method path (Record specWithDefaults)
      handler = handle route payloadHandler

class Handleable route handler | route -> handler where
  handle :: route -> handler -> List String -> HTTP.Request -> HTTP.Response -> Maybe (Effect Unit)

instance handleablePostRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , body :: body
           | r }
       , IsSymbol path
       , TypeEquals handlerReq { params :: Record params, body :: body }
       , IsRespondable res
       , PayloadUrl.DecodeUrl path params
       , SimpleJson.ReadForeign body
       , ParseUrl path urlParts
       , ToSegments urlParts
       )
    => Handleable (Route "POST" path (Record route)) (handlerReq -> Aff res) where
  handle route handler Nil req res =
    Just (sendError res (internalError "No path segments passed to handler"))
  handle route handler (method : pathSegments) req res =
    case decodePath pathSegments of
      Right params -> Just (handleRequest params)
      Left _ -> Nothing
    where
      decodePath = PayloadUrl.decodeUrl (SProxy :: SProxy path) (Proxy :: Proxy (Record params))
      handleRequest params = Aff.launchAff_ $ Aff.catchError (runRequest params) (sendInternalError res)
      runRequest :: Record params -> Aff Unit
      runRequest params = do
        bodyResult <- map SimpleJson.readJSON (readBody req)
        case bodyResult of
          Right body -> do
            let handlerReq = { params, body }
            handlerResp <- handler (from handlerReq)
            liftEffect $ sendResponse res handlerResp
          Left errors -> do
            sendInternalError res $ error $ "Error decoding body: " <> show errors

instance handleableGetRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , body :: body
           | r }
       , IsSymbol path
       , TypeEquals handlerReq { params :: Record params, body :: body }
       , IsRespondable res
       , PayloadUrl.DecodeUrl path params
       , SimpleJson.ReadForeign body
       , ParseUrl path urlParts
       , ToSegments urlParts
       )
    => Handleable (Route "GET" path (Record route)) (handlerReq -> Aff res) where
  handle route handler Nil req res =
    Just (sendError res (internalError "No path segments passed to handler"))
  handle route handler (method : pathSegments) req res =
    case decodePath pathSegments of
      Right params -> Just (handleRequest params)
      Left _ -> Nothing
    where
      decodePath = PayloadUrl.decodeUrl (SProxy :: SProxy path) (Proxy :: Proxy (Record params))
      handleRequest params = Aff.launchAff_ $ Aff.catchError (runRequest params) (sendInternalError res)

      runRequest :: Record params -> Aff Unit
      runRequest params = do
        let handlerReq = { params, body: unsafeCoerce {} }
        handlerResp <- handler (from handlerReq)
        liftEffect $ sendResponse res handlerResp

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
