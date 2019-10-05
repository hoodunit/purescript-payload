module Payload.Examples.Basic.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Utils as StringUtils
import Debug.Trace as Debug
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.HTTP as HTTP
import Payload.Client.Client (mkClient)
import Payload.Client.Client as Client
import Payload.Examples.Basic.Api (AdminUser(..), Post, User, api)
import Payload.Guards (GuardFn)
import Payload.Guards as Guards
import Payload.Handlers (File(..))
import Payload.Response (Failure(Forward))
import Payload.Server as Payload
import Payload.Spec (GET, Route, POST)
import Payload.Test.Helpers (withServer)
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest, runTestWith)
import Test.Unit.Output.Fancy as Fancy

getUsers :: forall r. { adminUser :: AdminUser | r } -> Aff (Array User)
getUsers { adminUser: AdminUser adminUser } = pure [adminUser, { id: 1, name: "John Doe" }]

getUsersNonAdmin :: forall r. { | r } -> Aff (Array User)
getUsersNonAdmin _ = pure [{ id: 1, name: "John Doe" }]

getUser :: forall r. { id :: Int | r } -> Aff User
getUser {id} = pure { id, name: "John Doe" }

getUsersProfiles :: forall r. { | r } -> Aff (Array String)
getUsersProfiles _ = pure ["Profile1", "Profile2"]

createUser :: forall r. { body :: User | r } -> Aff User
createUser {body: user} = pure user

getUserPost :: forall r. { postId :: String | r } -> Aff Post
getUserPost {postId} = pure { id: postId, text: "Some post" }

indexPage :: forall r. { | r} -> Aff File
indexPage _ = pure (File "test/index.html")

-- Exposes to directory traversal attack
files :: forall r. { path :: List String | r} -> Aff File
files { path } = pure (File $ "test/" <> String.joinWith "/" (Array.fromFoldable path))

getPage :: forall r. { id :: String | r} -> Aff String
getPage { id } = pure $ "Page " <> id

getPageMetadata :: forall r. { id :: String | r} -> Aff String
getPageMetadata { id } = pure $ "Page metadata " <> id

getHello :: forall r. { | r} -> Aff String
getHello _ = pure "Hello!"

assertResp :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertResp req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors
  
tests :: TestSuite
tests = do
  suite "Example: basic" do
    let client = mkClient Client.defaultOpts api
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

getAdminUser :: HTTP.Request -> Aff (Either Failure AdminUser)
getAdminUser req = do
  if StringUtils.endsWith "secret" (HTTP.requestURL req)
     then pure (Right (AdminUser { id: 1, name: "John Admin" }))
     else pure (Left (Forward "Not an admin"))

runTests :: Aff Unit
runTests = do
  let handlers = {
        users: {
           getUsersProfiles,
           userById: {
             getUser,
             getUserPost
           }
        },
        adminUsers: {
          getUsers,
          createUser
        },
        getUsersNonAdmin,
        indexPage,
        files,
        getPage,
        getPageMetadata,
        getHello
  }
  let guards = { adminUser: getAdminUser, request: Guards.request }
  withServer api { handlers, guards } (runTestWith Fancy.runTest tests)
