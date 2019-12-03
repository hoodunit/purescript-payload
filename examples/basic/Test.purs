module Payload.Examples.Basic.Test where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Payload.Client (mkClient, mkGuardedClient)
import Payload.Client as Client
import Payload.Examples.Basic.Main (api)
import Payload.Examples.Basic.Spec (spec)
import Payload.Headers as Headers
import Payload.Test.Config (TestConfig)
import Payload.Test.Helpers (bodyEquals, withServer)
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert

assertResp :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertResp req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors

assertFail :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> Test
assertFail req = do
  res <- req
  case res of
    Right val -> failure $ "Request succeeded with response " <> show val
    Left errors -> pure unit
  
tests :: TestConfig -> TestSuite
tests cfg = do
  let withApi = withServer spec api
  let client = mkGuardedClient cfg.clientOpts spec
  let authHeader = Tuple "Authorization" "Token secret"
  let authenticatedOpts = { headers: Headers.fromFoldable [ authHeader ] }
  suite "Example: basic" do
    test "GET /users (with secret)" $ withApi do
      res <- client.adminUsers.get_ authenticatedOpts {}
      bodyEquals [{ id: 1, name: "John Admin" }, { id: 1, name: "John Doe" }] res
    test "GET /users without secret should fall through to non-admin route" $ withApi do
      res <- client.getUsersNonAdmin { params: { name: "users" } }
      bodyEquals [{ id: 1, name: "John Doe" }] res
    test "GET /users/<id>" $ withApi do
      res <- client.users.byId.get { params: { id: 1 } }
      bodyEquals { id: 1, name: "whodunnit" } res
    test "GET /users/profile" $ withApi do
      res <- client.users.getProfiles {}
      bodyEquals ["Profile1", "Profile2"] res
    test "POST /users/new" $ withApi do
      res <- client.adminUsers.create_ authenticatedOpts { body: { id: 5, name: "New user!" }}
      bodyEquals { id: 5, name: "New user!" } res
    test "POST /users/new fails without the secret" $ withApi do
      assertFail (client.adminUsers.create { body: { id: 5, name: "New user!" }})
    test "GET /users/<id>/posts/<postId>" $ withApi $ do
      res <- client.users.byId.getPost { params: { id: 1, postId: "1" } }
      bodyEquals { id: "1", text: "Some post" } res
    test "GET /pages/<id>" $ withApi $ do
      res <- client.getPage { params: { id: "1" } }
      bodyEquals "Page 1" res
    test "GET /pages/<id>/metadata" $ withApi $ do
      res <- client.getPageMetadata { params: { id: "1" }}
      bodyEquals "Page metadata 1" res
    test "GET /hello%20there" $ withApi $ do
      res <- client.getHello {}
      bodyEquals "Hello!" res
