module Payload.Test.Helpers where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, error, throwError)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect)
import Payload.Client.Response (ClientResponse)
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.ResponseTypes (Response(..))
import Payload.Server as Payload
import Payload.Server.Routable (class Routable)
import Payload.Spec (Spec(Spec))
import Test.Unit (Test, failure, success)
import Test.Unit.Assert as Assert

withServer
  :: forall routesSpec guardsSpec handlers guards
  . Routable routesSpec guardsSpec handlers guards Aff
  => Spec { routes :: routesSpec, guards :: guardsSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff Unit
  -> Aff Unit
withServer = withServer' identity

withServer'
  :: forall routesSpec guardsSpec handlers guards m
   . MonadEffect m
  => Routable routesSpec guardsSpec handlers guards m
  => (m ~> Aff)
  -> Spec { routes :: routesSpec, guards :: guardsSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff Unit
  -> Aff Unit
withServer' runM apiSpec api_ aff = do
  let opts = Payload.defaultOpts { logLevel = Payload.LogError, port = 3000 }
  whileServerRuns (Payload.startGuarded' runM opts apiSpec api_) aff

whileServerRuns ::
  Aff (Either String Payload.Server)
  -> Aff Unit
  -> Aff Unit
whileServerRuns runServer doWhileRunning = do
  Aff.bracket runServer completed runAff
  pure unit
  where
    runAff (Left e) = Aff.throwError (Aff.error e)
    runAff (Right _) = doWhileRunning
    completed (Left _) = pure unit
    completed (Right server) = Payload.close server

withRoutes :: forall routesSpec handlers
  . Routable routesSpec {} handlers {} Aff
  => Spec routesSpec
  -> handlers
  -> Aff Unit
  -> Aff Unit
withRoutes = withRoutes' identity

withRoutes' :: forall routesSpec handlers m
  . MonadEffect m
  => Routable routesSpec {} handlers {} m
  => (m ~> Aff)
  -> Spec routesSpec
  -> handlers
  -> Aff Unit
  -> Aff Unit
withRoutes' runM _ handlers = 
  withServer' runM
              (Spec :: Spec { guards :: {}, routes :: routesSpec })
              { guards: {}, handlers }

type ApiResponse =
  { status :: Int
  , body :: String
  , headers :: Map String String }

type RequestClient =
  { get :: String -> Aff ApiResponse
  , options :: String -> Aff ApiResponse
  , post :: String -> String -> Aff ApiResponse
  , put :: String -> String -> Aff ApiResponse
  , delete :: String -> Maybe String -> Aff ApiResponse
  , head :: String -> Aff ApiResponse }

request :: String -> RequestClient
request host =
  { get: get host
  , options: options host
  , post: post host
  , put: put host
  , delete: delete host
  , head: head host }

get :: String -> String -> Aff ApiResponse
get host path = AX.get ResponseFormat.string (host <> "/" <> path) >>= decodeResponse

get_ :: String -> String -> Headers -> Aff ApiResponse
get_ host path headers = AX.request req >>= decodeResponse
  where
    req = AX.defaultRequest
            { method = Left GET
            , url = host <> "/" <> path
            , responseFormat = ResponseFormat.string
            , headers = (\(Tuple name val) -> RequestHeader name val) <$> Headers.toUnfoldable headers }

options :: String -> String -> Aff ApiResponse
options host path = do
  let url = host <> "/" <> path
  let req = AX.defaultRequest
        { method = Left OPTIONS
        , url = url
        , responseFormat = ResponseFormat.string }
  result <- AX.request req
  decodeResponse result

post :: String -> String -> String -> Aff ApiResponse
post host path reqBody = AX.post ResponseFormat.string (host <> path) (Just body) >>= decodeResponse
  where body = RequestBody.String reqBody

put :: String -> String -> String -> Aff ApiResponse
put host path reqBody = do
  let body = Just $ RequestBody.String reqBody
  result <- AX.put ResponseFormat.string (host <> "/" <> path) body
  decodeResponse result

delete :: String -> String -> Maybe String -> Aff ApiResponse
delete host path reqBody = do
  let content = RequestBody.String <$> reqBody
  let url = host <> "/" <> path
  let req = AX.defaultRequest
        { method = Left DELETE
        , url = url
        , content = content
        , responseFormat = ResponseFormat.string }
  result <- AX.request req
  decodeResponse result

head :: String -> String -> Aff ApiResponse
head host path = AX.request req >>= decodeResponse
  where
    req = AX.defaultRequest
      { method = Left HEAD
      , responseFormat = ResponseFormat.string
      , url = host <> "/" <> path }

decodeResponse :: Either AX.Error (AX.Response String) -> Aff ApiResponse
decodeResponse (Right res) = pure (decodeBody res)
decodeResponse (Left err) = throwError (error $ AX.printError err)

decodeBody :: AX.Response String -> ApiResponse
decodeBody res =
  { status: unwrapStatusCode res.status
  , body: res.body
  , headers: Map.fromFoldable $ unwrapHeader <$> res.headers }
  where
    unwrapHeader (ResponseHeader name value) = Tuple name value

unwrapStatusCode :: StatusCode -> Int
unwrapStatusCode (StatusCode c) = c

respMatches :: { status :: Int, body :: String } -> ApiResponse -> Test
respMatches expected received =
  Assert.equal expected { status: received.status, body: received.body }

bodyEquals :: forall body. Eq body => Show body => body -> ClientResponse body -> Aff Unit
bodyEquals expected (Right (Response { body })) = Assert.equal expected body
bodyEquals _ (Left err) = throwError (error $ "Expected body, received: " <> show err)

assertRes :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertRes req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors

assertOk :: forall a err. Show err => Aff (Either err a) -> Test
assertOk req = do
  res <- req
  case res of
    Right _ -> success
    Left errors -> failure $ "Request failed: " <> show errors

assertFail :: forall a err. Aff (Either err a) -> Test
assertFail req = do
  res <- req
  case res of
    Right _ -> failure $ "Expected failure but request succeeded"
    Left errors -> success
