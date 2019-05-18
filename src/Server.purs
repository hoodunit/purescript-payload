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
import Data.Symbol (SProxy(..))
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
import Record as Record

type Options =
  { backlog :: Maybe Int
  , hostname :: String
  , port :: Int
  , logLevel :: LogLevel }

data LogLevel = LogSilent | LogError | LogNormal | LogDebug

instance eqLogLevel :: Eq LogLevel where
  eq LogSilent LogSilent = true
  eq LogError LogError = true
  eq LogNormal LogNormal = true
  eq LogDebug LogDebug = true
  eq _ _ = false

instance ordLogLevel :: Ord LogLevel where
  compare l1 l2 = rank l1 `compare` rank l2
    where
      rank :: LogLevel -> Int
      rank LogSilent = 0
      rank LogError = 1
      rank LogNormal = 2
      rank LogDebug = 3

defaultOpts :: Options
defaultOpts =
  { backlog: Nothing
  , hostname: "localhost"
  , port: 3000
  , logLevel: LogNormal }

type PayloadServer = HTTP.Server

type Config =
  { logger :: Logger }

type Logger =
  { log :: String -> Effect Unit
  , logDebug :: String -> Effect Unit
  , logError :: String -> Effect Unit
  }

foreign import unsafeDecodeURIComponent :: String -> String

start_
  :: forall apiSpec handlers
   . Routable apiSpec handlers
  => apiSpec
  -> handlers
  -> Aff (Either String PayloadServer)
start_ = start defaultOpts

start
  :: forall apiSpec handlers
   . Routable apiSpec handlers
  => Options
  -> apiSpec
  -> handlers
  -> Aff (Either String PayloadServer)
start opts apiSpec handlers = do
  let cfg = mkConfig opts
  case mkRouter apiSpec handlers of
    Right routerTrie -> do
      server <- liftEffect $ HTTP.createServer (handleRequest cfg routerTrie)
      let httpOpts = Record.delete (SProxy :: SProxy "logLevel") opts
      listen cfg server httpOpts
      pure (Right server)
    Left err -> pure (Left err)

mkConfig :: Options -> Config
mkConfig { logLevel } = { logger: mkLogger logLevel }

mkLogger :: LogLevel -> Logger
mkLogger logLevel = { log: log_, logDebug, logError }
  where
    log_ :: String -> Effect Unit
    log_ | logLevel >= LogNormal = log
    log_ = const $ pure unit

    logDebug :: String -> Effect Unit
    logDebug | logLevel >= LogDebug = log
    logDebug = const $ pure unit

    logError :: String -> Effect Unit
    logError | logLevel >= LogError = log
    logError = const $ pure unit

handleRequest :: Config -> Trie HandlerEntry -> HTTP.Request -> HTTP.Response -> Effect Unit
handleRequest { logger } routerTrie req res = do
  let url = Url.parse (HTTP.requestURL req)
  logger.logDebug (HTTP.requestMethod req <> " " <> show (url.path))
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

listen :: Config -> HTTP.Server -> HTTP.ListenOptions -> Aff Unit
listen { logger } server opts = Aff.makeAff $ \cb -> do
  HTTP.listen server opts (logger.log startedMsg *> cb (Right unit))
  pure $ Aff.Canceler (\error -> liftEffect (logger.logError (errorMsg error)) *> close server)
  where
    startedMsg = "Listening on port " <> show opts.port
    errorMsg e = "Closing server due to error: " <> show e

close :: HTTP.Server -> Aff Unit
close server = Aff.makeAff $ \cb -> do
  HTTP.close server (cb (Right unit))
  pure Aff.nonCanceler
