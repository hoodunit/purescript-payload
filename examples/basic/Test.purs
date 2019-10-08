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
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy

assertResp :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertResp req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors
  
tests :: TestSuite
tests = do
  suite "Example: basic" do
    let client = mkClient Client.defaultOpts spec
    test "GET /users (with secret)" $ assertResp
      (client.adminUsers.getUsers (\r -> r { url = r.url <> "?secret" }) {})
      [{ id: 1, name: "John Admin" }, { id: 1, name: "John Doe" }]
    test "GET /users without secret should fall through to non-admin route" $ assertResp
      (client.getUsersNonAdmin identity { name: "users" })
      [{ id: 1, name: "John Doe" }]
    test "GET /users/<id>" $ assertResp
      (client.users.userById.getUser identity { id: 1 })
      { id: 1, name: "John Doe" }
    test "GET /users/profile" $ assertResp
      (client.users.getUsersProfiles identity {})
      ["Profile1", "Profile2"]
    test "POST /users/new" $ do
      let opts = \r -> r { url = r.url <> "?secret" }
      assertResp
        (client.adminUsers.createUser opts { body: { id: 5, name: "New user!" }})
        { id: 5, name: "New user!" }
    -- test "POST /users/new fails without the secret" $ do
    --   let opts = Client.defaultOpts
    --   assertResp
    --     (Client.request opts api.createUser { body: { id: 5, name: "New user!" }})
    --     { id: 5, name: "New user!" }
    test "GET /users/<id>/posts/<postId>" $ assertResp
      (client.users.userById.getUserPost identity { id: 1, postId: "1" })
      { id: "1", text: "Some post" }
    test "GET /pages/<id>" $ assertResp
      (client.getPage identity { id: "1" })
      "Page 1"
    test "GET /pages/<id>/metadata" $ assertResp
      (client.getPageMetadata identity { id: "1"})
      "Page metadata 1"
    test "GET /hello%20there" $ assertResp
      (client.getHello identity {})
      "Hello!"

runTests :: Aff Unit
runTests = do
  withServer spec api (runTestWith Fancy.runTest tests)
