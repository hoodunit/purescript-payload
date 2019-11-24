module Payload.Test.Integration.Client.Errors where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)
import Payload.Client (mkClient)
import Payload.Client.Queryable (ClientError(..), ClientResponse)
import Payload.Headers as Headers
import Payload.ResponseTypes (Response(..))
import Payload.Server.Response as Response
import Payload.Server.Status as Status
import Payload.Spec (GET, Spec(Spec))
import Payload.Test.Config (TestConfig)
import Payload.Test.Helpers (withRoutes)
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert as Assert
  
tests :: TestConfig -> TestSuite
tests cfg = do
  suite "Errors" do
    suite "StatusError" do
      test "response includes status" $ do
        let spec = Spec :: _ { foo :: GET "/foo" { response :: { id :: Int } } }
        let handlers = { foo: \_ -> pure (Left "Fail!" :: Either String { id :: Int }) }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          case res of
            Left (StatusError { response: Response response }) -> Assert.equal 500 response.status.code
            r -> throwError (error $ "Expected StatusError but received " <> show r)
      test "response includes headers" $ do
        let spec = Spec :: _ { foo :: GET "/foo" { response :: { id :: Int } } }
        let handlers = { foo: \_ -> pure (Left "Fail!" :: Either String { id :: Int }) }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          case res of
            Left (StatusError { response: Response response }) -> do
              Assert.equal (Just "5") (Headers.lookup "content-length" response.headers)
            r -> throwError (error $ "Expected StatusError but received " <> show r)
      test "response includes body" $ do
        let spec = Spec :: _ { foo :: GET "/foo" { response :: { id :: Int } } }
        let handlers = { foo: \_ -> pure (Left "Fail!" :: Either String { id :: Int }) }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          case res of
            Left (StatusError { response: Response response }) -> do
              Assert.equal "Fail!" response.body
            r -> throwError (error $ "Expected StatusError but received " <> show r)
    suite "DecodeError" do
      test "200 response that fails to decode returns decode error with body" $ do
        let spec = Spec :: _ { foo :: GET "/foo" { response :: { id :: Int } } }
        let handlers = { foo: \_ -> pure ((Left (Response.ok "Fail!")) :: Either (Response String) { id :: Int }) }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          case res of
            Left (DecodeError { response: Response response }) -> do
              Assert.equal "Fail!" response.body
            r -> throwError (error $ "Expected DecodeError but received " <> show r)
