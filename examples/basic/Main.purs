module Payload.Examples.Basic.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Utils as StringUtils
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.HTTP as HTTP
import Payload.Client as Client
import Payload.Examples.Basic.Api (AdminUser(..), Post, User, api)
import Payload.Guards (GuardFn)
import Payload.Guards as Guards
import Payload.Handlers (File(..))
import Payload.Route (GET, Route(..), POST)
import Payload.Server as Payload
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
    test "GET /users (with secret)" $ assertResp
      (Client.request (Client.defaultOpts { query = Just "secret" }) api.routes.getUsers {})
      [{ id: 1, name: "John Admin" }, { id: 1, name: "John Doe" }]
    test "GET /users without secret should fall through to non-admin route" $ assertResp
      (Client.request_ api.routes.getUsersNonAdmin { name: "users" })
      [{ id: 1, name: "John Doe" }]
    test "GET /users/<id>" $ assertResp
      (Client.request_ api.routes.getUser { id: 1 })
      { id: 1, name: "John Doe" }
    test "GET /users/profile" $ assertResp
      (Client.request_ api.routes.getUsersProfiles {})
      ["Profile1", "Profile2"]
    test "POST /users/new" $ do
      let opts = Client.defaultOpts { query = Just "secret" }
      assertResp
        (Client.request opts api.routes.createUser { body: { id: 5, name: "New user!" }})
        { id: 5, name: "New user!" }
    -- test "POST /users/new fails without the secret" $ do
    --   let opts = Client.defaultOpts
    --   assertResp
    --     (Client.request opts api.createUser { body: { id: 5, name: "New user!" }})
    --     { id: 5, name: "New user!" }
    test "GET /users/<id>/posts/<postId>" $ assertResp
      (Client.request_ api.routes.getUserPost { id: 1, postId: "1" })
      { id: "1", text: "Some post" }
    test "GET /pages/<id>" $ assertResp
      (Client.request_ api.routes.getPage { id: "1" })
      "Page 1"
    test "GET /pages/<id>/metadata" $ assertResp
      (Client.request_ api.routes.getPageMetadata { id: "1"})
      "Page metadata 1"
    test "GET /hello%20there" $ assertResp
      (Client.request_ api.routes.getHello {})
      "Hello!"

getAdminUser :: GuardFn AdminUser
getAdminUser req = do
  if StringUtils.endsWith "secret" (HTTP.requestURL req)
     then pure (Right (AdminUser { id: 1, name: "John Admin" }))
     else pure (Left "Fail not an admin")

startTestServer :: Aff Unit
startTestServer = do
  let opts = Payload.defaultOpts { logLevel = Payload.LogError }
  let handlers = { getUsers
                 , getUsersNonAdmin
                 , getUser
                 , getUsersProfiles
                 , createUser
                 , getUserPost
                 , indexPage
                 , files
                 , getPage
                 , getPageMetadata
                 , getHello }
  let guards = { adminUser: getAdminUser, request: Guards.request }
  startResult <- Payload.start opts api { handlers, guards }
  case startResult of
    Right _ -> pure unit
    Left err -> liftEffect (log $ "Could not start test server: " <> err)

runTests :: Aff Unit
runTests = do
  startTestServer
  runTestWith Fancy.runTest tests
