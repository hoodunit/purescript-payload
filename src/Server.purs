module Payload.Server where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Nullable (toMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.HTTP as HTTP
import Node.URL (URL)
import Node.URL as Url
import Payload.Response (sendError)
import Payload.Routing (class Routable, HandlerEntry, mkRouter)
import Payload.Trie (Trie)
import Payload.Trie as Trie

type Options =
  { hostname :: String
  , port :: Int
  , backlog :: Maybe Int
  }

type PayloadServer = HTTP.Server

foreign import unsafeDecodeURIComponent :: String -> String

start
  :: forall apiSpec handlers
   . Routable apiSpec handlers
  => Options
  -> apiSpec
  -> handlers
  -> Aff (Either String PayloadServer)
start opts apiSpec handlers = do
  case mkRouter apiSpec handlers of
    Right routerTrie -> do
      server <- liftEffect $ HTTP.createServer (handleRequest routerTrie)
      listen server opts
      pure (Right server)
    Left err -> pure (Left err)

handleRequest :: Trie HandlerEntry -> HTTP.Request -> HTTP.Response -> Effect Unit
handleRequest routerTrie req res = do
  let url = Url.parse (HTTP.requestURL req)
  -- log (HTTP.requestMethod req <> " " <> show (url.path))
  case requestSegments req of
    Right reqSegments -> runHandlers routerTrie reqSegments req res
    Left err -> sendError res { status: 500,
                                statusMsg: "Internal Error",
                                body: "Path could not be decoded: " <> show err }

runHandlers :: Trie HandlerEntry -> List String -> HTTP.Request -> HTTP.Response -> Effect Unit
runHandlers routerTrie pathSegments req res = do
  let matches = Trie.lookup pathSegments routerTrie
  case foldl handleNext Nothing matches of
    Just handleIt -> handleIt
    Nothing -> sendError res { status: 404, statusMsg: "Not Found", body: "" }
  where
    handleNext :: Maybe (Effect Unit) -> HandlerEntry -> Maybe (Effect Unit)
    handleNext (Just e) _ = Just e
    handleNext Nothing { handler } = handler pathSegments req res
  
requestSegments :: HTTP.Request -> Either String (List String)
requestSegments req = do
  path <- requestPath req
  let segments = pathToSegments path
  let decodedSegments = unsafeDecodeURIComponent <$> segments
  pure (method : decodedSegments)
  where
    method = HTTP.requestMethod req
  
requestPath :: HTTP.Request -> Either String String
requestPath = HTTP.requestURL >>> Url.parse >>> urlPath

urlPath :: URL -> Either String String
urlPath url = url.pathname
  # toMaybe
  # maybe (Left "No path") Right

pathToSegments :: String -> List String
pathToSegments = dropEmpty <<< List.fromFoldable <<< String.split (wrap "/")
  where
    dropEmpty ("" : xs) = dropEmpty xs
    dropEmpty xs = xs

listen :: HTTP.Server -> HTTP.ListenOptions -> Aff Unit
listen server opts = Aff.makeAff $ \cb -> do
  HTTP.listen server opts (logStarted *> cb (Right unit))
  pure $ Aff.Canceler (\error -> liftEffect (logError error) *> close server)
  where
    logStarted = log $ "Listening on port " <> show opts.port
    logError e = log $ "Closing server due to error: " <> show e

close :: HTTP.Server -> Aff Unit
close server = Aff.makeAff $ \cb -> do
  HTTP.close server (cb (Right unit))
  pure Aff.nonCanceler
