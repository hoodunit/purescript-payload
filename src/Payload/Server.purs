module Payload.Server where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Nullable (toMaybe)
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.HTTP as HTTP
import Node.URL (URL)
import Node.URL as Url
import Payload.Internal.Trie (Trie)
import Payload.Internal.Trie as Trie
import Payload.Request (RequestUrl)
import Payload.Response (ResponseBody(..), internalError, writeResponse)
import Payload.Response as Response
import Payload.Routable (class Routable, API(API), HandlerEntry, Outcome(..), mkRouter)
import Payload.Status as Status
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

start
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {}
  => Options
  -> API routesSpec
  -> handlers
  -> Aff (Either String PayloadServer)
start opts routeSpec handlers = startGuarded opts api { handlers, guards: {} }
  where
    api = API :: API { routes :: routesSpec, guards :: {} }

start_
  :: forall routesSpec handlers
   . Routable routesSpec {} handlers {}
  => API routesSpec
  -> handlers
  -> Aff (Either String PayloadServer)
start_ = start defaultOpts

startGuarded_
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards
  => API { routes :: routesSpec, guards :: guardsSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff (Either String PayloadServer)
startGuarded_ = startGuarded defaultOpts

startGuarded
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards
  => Options
  -> API { guards :: guardsSpec, routes :: routesSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff (Either String PayloadServer)
startGuarded opts apiSpec api = do
  let cfg = mkConfig opts
  case mkRouter apiSpec api of
    Right routerTrie -> do
      server <- liftEffect $ HTTP.createServer (handleRequest cfg routerTrie)
      let httpOpts = Record.delete (SProxy :: SProxy "logLevel") opts
      listenResult <- listen cfg server httpOpts
      pure (const server <$> listenResult)
    Left err -> pure (Left err)

dumpRoutes :: Trie HandlerEntry -> Effect Unit
dumpRoutes routerTrie = log $ Trie.dumpEntries (_.route <$> routerTrie)

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
  case requestUrl req of
    Right reqUrl -> runHandlers routerTrie reqUrl req res
    Left err -> do
      writeResponse res (internalError $ "Path could not be decoded: " <> show err)

runHandlers :: Trie HandlerEntry -> RequestUrl -> HTTP.Request -> HTTP.Response -> Effect Unit
runHandlers routerTrie reqUrl req res = do
  let (matches :: List HandlerEntry) = Trie.lookup (reqUrl.method : reqUrl.path) routerTrie
  Aff.launchAff_ $ do
    outcome <- handleNext (Forward "Dummy forward") matches
    case outcome of
      (Forward _) -> liftEffect $ writeResponse res (Response.status Status.notFound (StringBody ""))
      _ -> pure unit
  where
    handleNext :: Outcome -> List HandlerEntry -> Aff Outcome
    handleNext Success _ = pure Success
    handleNext Failure _ = pure Failure
    handleNext (Forward msg) ({ handler } : rest) =
      handler reqUrl req res >>= \o -> handleNext o rest
    handleNext _ Nil = pure (Forward "No match could handle")
  
requestUrl :: HTTP.Request -> Either String RequestUrl
requestUrl req = do
  let parsedUrl = Url.parse (HTTP.requestURL req)
  path <- urlPath parsedUrl
  let query = fromMaybe "" $ toMaybe parsedUrl.query
  let pathSegments = urlToSegments path
  pure { method, path: pathSegments, query }
  where
    method = HTTP.requestMethod req

urlPath :: URL -> Either String String
urlPath url = url.pathname
  # toMaybe
  # maybe (Left "No path") Right

urlQuery :: URL -> Maybe String
urlQuery url = url.query # toMaybe

urlToSegments :: String -> List String
urlToSegments = pathToSegments >>> (map unsafeDecodeURIComponent)

pathToSegments :: String -> List String
pathToSegments = dropEmpty <<< List.fromFoldable <<< String.split (wrap "/")
  where
    dropEmpty ("" : xs) = dropEmpty xs
    dropEmpty xs = xs

foreign import onError :: HTTP.Server -> (Error -> Effect Unit) -> Effect Unit

listen :: Config -> HTTP.Server -> HTTP.ListenOptions -> Aff (Either String Unit)
listen { logger } server opts = Aff.makeAff $ \cb -> do
  onError server \error -> cb (Right (Left (show error)))
  HTTP.listen server opts (logger.log startedMsg *> cb (Right (Right unit)))
  pure $ Aff.Canceler (\error -> liftEffect (logger.logError (errorMsg error)) *> close server)
  where
    startedMsg = "Listening on port " <> show opts.port
    errorMsg e = "Closing server due to error: " <> show e

close :: PayloadServer -> Aff Unit
close server = Aff.makeAff $ \cb -> do
  HTTP.close server (cb (Right unit))
  pure Aff.nonCanceler
