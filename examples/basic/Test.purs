module Payload.Examples.Basic.Test where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Payload.Client (mkGuardedClient, unwrapBody, unwrapResponse)
import Payload.Examples.Basic.Main (api)
import Payload.Examples.Basic.Spec (spec)
import Payload.Headers as Headers
import Payload.Test.Config (TestConfig)
import Payload.Test.Helpers (withServer)
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
  let authenticatedOpts = { extraHeaders: Headers.fromFoldable [ authHeader ] }
  suite "Example: basic" do
    test "GET /users (with secret)" $ withApi do
      users <- unwrapBody $ client.adminUsers.get_ authenticatedOpts {}
      Assert.equal users [{ id: 1, name: "John Admin" }, { id: 1, name: "John Doe" }]
    test "GET /users without secret should fall through to non-admin route" $ withApi do
      users <- unwrapBody $ client.getUsersNonAdmin { params: { name: "users" } }
      Assert.equal [{ id: 1, name: "John Doe" }] users
    test "GET /users/<id>" $ withApi do
      user <- unwrapBody $ client.users.byId.get { params: { id: 1 } }
      Assert.equal { id: 1, name: "whodunnit" } user
    test "GET /users/profile" $ withApi do
      profiles <- unwrapBody $ client.users.getProfiles {}
      Assert.equal ["Profile1", "Profile2"] profiles
    test "POST /users/new" $ withApi do
      newUser <- unwrapBody $ client.adminUsers.create_ authenticatedOpts { body: { id: 5, name: "New user!" }}
      Assert.equal { id: 5, name: "New user!" } newUser
    test "POST /users/new fails without the secret" $ withApi do
      assertFail (client.adminUsers.create { body: { id: 5, name: "New user!" }})
    test "GET /users/<id>/posts/<postId>" $ withApi $ do
      post <- unwrapBody $ client.users.byId.getPost { params: { id: 1, postId: "1" } }
      Assert.equal { id: "1", text: "Some post" } post
    test "GET /pages/<id>" $ withApi $ do
      page <- unwrapBody $ client.getPage { params: { id: "1" } }
      Assert.equal "Page 1" page
    test "GET /pages/<id>/metadata" $ withApi $ do
      pageMetadata <- unwrapBody $ client.getPageMetadata { params: { id: "1" }}
      Assert.equal "Page metadata 1" pageMetadata
    test "GET /hello%20there" $ withApi $ do
      helloMsg <- unwrapBody $ client.getHello {}
      Assert.equal "Hello!" helloMsg
    test "GET /hello%20there (Response only)" $ withApi $ do
      res <- unwrapResponse $ client.getHello {}
      Assert.equal "Hello!" res.body
      Assert.equal 200 res.status.code
    test "GET /hello%20there (Response or error)" $ withApi $ do
      res <- client.getHello {}
      Assert.equal (Right "Hello!") ((_.body <<< unwrap) <$> res)
      Assert.equal (Right 200) ((_.status.code <<< unwrap) <$> res)
