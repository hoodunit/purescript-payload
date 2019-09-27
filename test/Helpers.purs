module Payload.Test.Helpers where

import Prelude

import Affjax as AX
import Affjax.RequestBody as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Routable (class Routable, API(..))
import Payload.Server as Payload
import Test.Unit (Test, failure, success)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))

withServer
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards
  => API { routes :: routesSpec, guards :: guardsSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff Unit
  -> Aff Unit
withServer apiSpec api_ aff = do
  let opts = Payload.defaultOpts { logLevel = Payload.LogError, port = 3000 }
  Aff.bracket (Payload.start opts apiSpec api_) completed runAff
  pure unit
  where
    runAff (Left _) = pure unit
    runAff (Right _) = aff
    completed (Left err) = liftEffect $ log ("Could not start server: " <> err)
    completed (Right server) = Payload.close server

withRoutes :: forall routesSpec handlers
  . Routable routesSpec {} handlers {}
  => Proxy routesSpec
  -> handlers
  -> Aff Unit
  -> Aff Unit
withRoutes _ handlers = 
  withServer (API :: API { guards :: {}, routes :: routesSpec })
             { guards: {}, handlers }

type ApiResponse =
  { status :: Int
  , body :: String
  , headers :: Map String String }

type RequestClient =
  { get :: String -> Aff ApiResponse
  , post :: String -> String -> Aff ApiResponse
  , put :: String -> String -> Aff ApiResponse
  , delete :: String -> Aff ApiResponse
  , head :: String -> Aff ApiResponse }

request :: String -> RequestClient
request host =
  { get: get host
  , post: post host
  , put: put host
  , delete: delete host
  , head: head host }

get :: String -> String -> Aff ApiResponse
get host path = decodeBody <$> AX.get ResponseFormat.string (host <> "/" <> path)

post :: String -> String -> String -> Aff ApiResponse
post host path reqBody = decodeBody <$> AX.post ResponseFormat.string (host <> path) body
  where body = RequestBody.String reqBody

put :: String -> String -> String -> Aff ApiResponse
put host path reqBody = decodeBody <$> AX.put ResponseFormat.string (host <> "/" <> path) (RequestBody.String reqBody)

delete :: String -> String -> Aff ApiResponse
delete host path = decodeBody <$> AX.delete ResponseFormat.string (host <> "/" <> path)

head :: String -> String -> Aff ApiResponse
head host path = decodeBody <$> AX.request req
  where
    req = AX.defaultRequest
      { method = Left HEAD
      , responseFormat = ResponseFormat.string
      , url = host <> "/" <> path }

decodeBody :: AX.Response (Either AX.ResponseFormatError String) -> ApiResponse
decodeBody res =
  { status: unwrapStatusCode res.status
  , body: either ResponseFormat.printResponseFormatError identity res.body
  , headers: Map.fromFoldable $ unwrapHeader <$> res.headers }
  where
    unwrapHeader (ResponseHeader name value) = Tuple name value

unwrapStatusCode :: StatusCode -> Int
unwrapStatusCode (StatusCode c) = c

respMatches :: { status :: Int, body :: String } -> ApiResponse -> Test
respMatches expected received =
  Assert.equal expected { status: received.status, body: received.body }

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
