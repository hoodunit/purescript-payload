module Payload.Examples.Test.Helpers where

import Prelude

import Affjax.Node as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, error, throwError)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Payload.Client.Response (ClientResponse)
import Payload.Examples.Test.Config as Config
import Payload.ResponseTypes (Response(..))
import Payload.Server as Payload
import Payload.Server.Routable (class Routable)
import Payload.Spec (Spec(Spec))
import Test.Unit (Test, failure, success)
import Test.Unit.Assert as Assert

withServer
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards
  => Spec { routes :: routesSpec, guards :: guardsSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff Unit
  -> Aff Unit
withServer apiSpec api_ aff = do
  let opts = Payload.defaultOpts { logLevel = Payload.LogError, port = Config.testPort }
  Aff.bracket (Payload.startGuarded opts apiSpec api_) completed runAff
  where
    runAff (Left e) = Aff.throwError (Aff.error e)
    runAff (Right _) = aff
    completed (Left _) = pure unit
    completed (Right server) = do
      liftEffect $ Payload.closeAllConnections server
      Payload.close server
      Aff.delay (Aff.Milliseconds 10.0)

withRoutes :: forall routesSpec handlers
  . Routable routesSpec {} handlers {}
  => Spec routesSpec
  -> handlers
  -> Aff Unit
  -> Aff Unit
withRoutes _ handlers =
  withServer (Spec :: Spec { guards :: {}, routes :: routesSpec })
             { guards: {}, handlers }

type ApiResponse =
  { status :: Int
  , body :: String
  , headers :: Map String String }

request :: String -> { get :: String -> Aff ApiResponse }
request host = { get: get host }

get :: String -> String -> Aff ApiResponse
get host path = AX.get ResponseFormat.string (joinUrl host path) >>= decodeResponse

joinUrl :: String -> String -> String
joinUrl host path
  | String.take 1 path == "/" = host <> path
  | otherwise = host <> "/" <> path

decodeResponse :: Either AX.Error (AX.Response String) -> Aff ApiResponse
decodeResponse (Right res) = pure
  { status: unwrapStatusCode res.status
  , body: res.body
  , headers: Map.fromFoldable $ unwrapHeader <$> res.headers }
  where
    unwrapStatusCode (StatusCode c) = c
    unwrapHeader (ResponseHeader name value) = Tuple name value
decodeResponse (Left err) = throwError (error $ AX.printError err)

respMatches :: { status :: Int, body :: String } -> ApiResponse -> Test
respMatches expected received =
  Assert.equal expected { status: received.status, body: received.body }

bodyEquals :: forall body. Eq body => Show body => body -> ClientResponse body -> Aff Unit
bodyEquals expected (Right (Response { body })) = Assert.equal expected body
bodyEquals _ (Left err) = throwError (error $ "Expected body, received: " <> show err)

assertFail :: forall a err. Aff (Either err a) -> Test
assertFail req = do
  res <- req
  case res of
    Right _ -> failure "Expected failure but request succeeded"
    Left _ -> success
