module Payload.Test.Integration.Client.Statuses where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, error, throwError)
import Payload.Client (ClientError(..), ClientResponse, mkClient)
import Payload.Server.Response as Response
import Payload.Spec (GET, Spec(Spec))
import Payload.Test.Config (TestConfig)
import Payload.Test.Helpers (bodyEquals, withRoutes)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

assertErrorRes :: forall body. ClientResponse body -> Aff ClientError
assertErrorRes (Right _) = throwError (error "Expected error")
assertErrorRes (Left err) = pure err

isStatusError :: ClientError -> Boolean
isStatusError (StatusError _) = true
isStatusError _ = false
  
tests :: TestConfig -> TestSuite
tests cfg = do
  suite "Statuses" do
    test "client decodes default (200 OK) response as response type" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: { id :: Int } } }
      let handlers = { foo: \_ -> pure {id: 123} }
      withRoutes spec handlers do
        let client = mkClient cfg.clientOpts spec
        res <- client.foo {}
        bodyEquals { id: 123 } res
    test "client decodes overridden 201 Created response as response type" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: { id :: Int } } }
      let handlers = { foo: \_ -> pure (Response.created {id: 123}) }
      withRoutes spec handlers do
        let client = mkClient cfg.clientOpts spec
        res <- client.foo {}
        bodyEquals { id: 123 } res
    test "client returns overridden 400 Bad Request response as error" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: { id :: Int } } }
      let handlers = { foo: \_ -> pure (Response.badRequest {id: 123}) }
      withRoutes spec handlers do
        let client = mkClient cfg.clientOpts spec
        res <- client.foo {}
        errorRes <- assertErrorRes res
        Assert.assert "Expected status error" (isStatusError errorRes)
