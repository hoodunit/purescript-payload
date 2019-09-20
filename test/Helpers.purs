module Payload.Test.Helpers where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Routable (class Routable, API(..))
import Payload.Server as Payload
import Test.Unit (Test, failure, success)
import Test.Unit.Assert as Assert

withServer
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards
  => API { routes :: routesSpec, guards :: guardsSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff Unit
  -> Aff Unit
withServer apiSpec api_ aff = do
  let opts = Payload.defaultOpts { logLevel = Payload.LogError }
  Aff.bracket (Payload.start opts apiSpec api_) completed runAff
  pure unit
  where
    runAff (Left _) = pure unit
    runAff (Right _) = aff
    completed (Left err) = liftEffect $ log ("Could not start server: " <> err)
    completed (Right server) = Payload.close server

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
