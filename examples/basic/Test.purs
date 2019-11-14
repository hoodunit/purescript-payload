module Payload.Examples.Basic.Test where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Payload.Client.Client (mkClient, mkGuardedClient)
import Payload.Client.Client as Client
import Payload.Examples.Basic.Main (api)
import Payload.Examples.Basic.Spec (spec)
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
  let authHeader = RequestHeader "Authorization" "Token secret"
  let addAuthHeader req = req { headers = req.headers <> [authHeader] }
  suite "Example: basic" do
    test "GET /users (with secret)" $ withApi do
      assertResp (client.adminUsers.getUsers addAuthHeader {})
        [{ id: 1, name: "John Admin" }, { id: 1, name: "John Doe" }]
    test "GET /users without secret should fall through to non-admin route" $ withApi do
      assertResp (client.getUsersNonAdmin identity { params: { name: "users" } })
        [{ id: 1, name: "John Doe" }]
    test "GET /users/<id>" $ withApi do
      assertResp (client.users.userById.getUser identity { params: { id: 1 } })
        { id: 1, name: "whodunnit" }
    test "GET /users/profile" $ withApi do
      assertResp (client.users.getUsersProfiles identity {})
        ["Profile1", "Profile2"]
    test "POST /users/new" $ withApi do
      assertResp
        (client.adminUsers.createUser addAuthHeader { body: { id: 5, name: "New user!" }})
        { id: 5, name: "New user!" }
    test "POST /users/new fails without the secret" $ withApi do
      assertFail (client.adminUsers.createUser identity { body: { id: 5, name: "New user!" }})
    test "GET /users/<id>/posts/<postId>" $ withApi $ assertResp
      (client.users.userById.getUserPost identity { params: { id: 1, postId: "1" } })
      { id: "1", text: "Some post" }
    test "GET /pages/<id>" $ withApi $ assertResp
      (client.getPage identity { params: { id: "1" } })
      "Page 1"
    test "GET /pages/<id>/metadata" $ withApi $ assertResp
      (client.getPageMetadata identity { params: { id: "1" }})
      "Page metadata 1"
    test "GET /hello%20there" $ withApi $ assertResp
      (client.getHello identity {})
      "Hello!"
