module Payload.Examples.Basic.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.String.Utils as StringUtils
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.HTTP as HTTP
import Payload.Client as Client
import Payload.Examples.Basic.Api (AdminUser(..), Post, User, api)
import Payload.Handlers (File(..))
import Payload.Route (GET, Route(..), POST)
import Payload.Server as Payload
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest, runTestWith)
import Test.Unit.Output.Fancy as Fancy

-- getUsers :: forall r. { adminUser :: AdminUser | r } -> Aff (Array User)
getUsers { adminUser: AdminUser adminUser } = pure [adminUser, { id: 1, name: "John Doe" }]

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
    test "GET /users" $ assertResp
      (Client.request api.getUsers {})
      [{ id: 1, name: "John Admin" }, { id: 1, name: "John Doe" }]
    test "GET /users" $ assertResp
      (Client.request api.getUsersNonAdmin {})
      [{ id: 1, name: "John Doe" }]
    test "GET /users/<id>" $ assertResp
      (Client.request api.getUser { id: 1 })
      { id: 1, name: "John Doe" }
    test "GET /users/profile" $ assertResp
      (Client.request api.getUsersProfiles {})
      ["Profile1", "Profile2"]
    -- test "POST /users/new" $ assertResp
    --   (Client.request api.createUser { body: { id: 5, name: "New user!" }})
    --   { id: 5, name: "New user!" }
    test "GET /users/<id>/posts/<postId>" $ assertResp
      (Client.request api.getUserPost { id: 1, postId: "1" })
      { id: "1", text: "Some post" }
    test "GET /pages/<id>" $ assertResp
      (Client.request api.getPage { id: "1" })
      "Page 1"
    test "GET /pages/<id>/metadata" $ assertResp
      (Client.request api.getPageMetadata { id: "1"})
      "Page metadata 1"
    test "GET /hello%20there" $ assertResp
      (Client.request api.getHello {})
      "Hello!"

getAdminUser :: HTTP.Request -> Aff (Either String AdminUser)
getAdminUser req = do
  if StringUtils.endsWith "secret" (HTTP.requestURL req)
     then pure (Left "Fail not an admin")
     else pure (Right (AdminUser { id: 1, name: "John Admin" }))

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
  let guards = { adminUser: getAdminUser }
  startResult <- Payload.start opts api { handlers, guards }
  case startResult of
    Right _ -> pure unit
    Left err -> liftEffect (log err)

runTests :: Aff Unit
runTests = do
  startTestServer
  runTestWith (Fancy.runTest) tests
