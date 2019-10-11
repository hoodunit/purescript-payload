module Payload.Examples.Basic.Test where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Payload.Client.Client (mkClient)
import Payload.Client.Client as Client
import Payload.Examples.Basic.Api (spec)
import Payload.Examples.Basic.Main (api)
import Payload.Test.Helpers (withServer)
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert

assertResp :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertResp req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors
  
tests :: TestSuite
tests = do
  let withApi = withServer spec api
  let client = mkClient Client.defaultOpts spec
  suite "Example: basic" do
    test "GET /users (with secret)" $ withApi do
      assertResp (client.adminUsers.getUsers (\r -> r { url = r.url <> "?secret" }) {})
        [{ id: 1, name: "John Admin" }, { id: 1, name: "John Doe" }]
    test "GET /users without secret should fall through to non-admin route" $ withApi do
      assertResp (client.getUsersNonAdmin identity { name: "users" })
        [{ id: 1, name: "John Doe" }]
    test "GET /users/<id>" $ withApi do
      assertResp (client.users.userById.getUser identity { id: 1 })
        { id: 1, name: "John Doe" }
    test "GET /users/profile" $ withApi do
      assertResp (client.users.getUsersProfiles identity {})
        ["Profile1", "Profile2"]
    test "POST /users/new" $ withApi do
      let opts = \r -> r { url = r.url <> "?secret" }
      assertResp
        (client.adminUsers.createUser opts { body: { id: 5, name: "New user!" }})
        { id: 5, name: "New user!" }
    -- test "POST /users/new fails without the secret" $ do
    --   let opts = Client.defaultOpts
    --   assertResp
    --     (Client.request opts api.createUser { body: { id: 5, name: "New user!" }})
    --     { id: 5, name: "New user!" }
    test "GET /users/<id>/posts/<postId>" $ withApi $ assertResp
      (client.users.userById.getUserPost identity { id: 1, postId: "1" })
      { id: "1", text: "Some post" }
    test "GET /pages/<id>" $ withApi $ assertResp
      (client.getPage identity { id: "1" })
      "Page 1"
    test "GET /pages/<id>/metadata" $ withApi $ assertResp
      (client.getPageMetadata identity { id: "1"})
      "Page metadata 1"
    test "GET /hello%20there" $ withApi $ assertResp
      (client.getHello identity {})
      "Hello!"
