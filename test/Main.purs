module Payload.Test.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Client as Client
import Payload.Handlers (File(..))
import Payload.Routing (GET, Route(..), POST)
import Payload.Server as Payload
import Payload.Test.Params as ParamsTest
import Payload.Test.Routing as RoutingTest
import Payload.Test.Trie as TrieTest
import Payload.Test.UrlParsing as UrlParsingTest
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

type User =
  { id :: Int
  , name :: String }

type Post =
  { id :: String
  , text :: String }

api =
  { getUsers: Route :: GET "/users"
      { response :: Array User }
  , getUser: Route :: GET "/users/<id>"
      { params :: { id :: Int }
      , response :: User }
  , getUsersProfiles: Route :: GET "/users/profiles"
      { response :: Array String }
  , createUser: Route :: POST "/users/new"
      { body :: User
      , response :: User }
  , getUserPost: Route :: GET "/users/<id>/posts/<postId>"
      { params :: { id :: Int, postId :: String }
      , response :: Post }
  , indexPage: Route :: GET "/"
      { response :: File }
  , files: Route :: GET "/<..path>"
      { params :: { path :: List String }
      , response :: File }
  , getPage: Route :: GET "/pages/<id>"
      { params :: { id :: String }
      , response :: String }
  , getPageMetadata: Route :: GET "/pages/<id>/metadata"
      { params :: { id :: String }
      , response :: String }
  }

handlers = { getUsers, getUser, getUsersProfiles, createUser, getUserPost, indexPage, files, getPage, getPageMetadata }

getUsers :: forall r. { | r } -> Aff (Array User)
getUsers _ = pure [{ id: 1, name: "John Doe" }]

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

assertResp :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertResp req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors
  
serverTests :: TestSuite
serverTests = do
  suite "Client query requests" do
    test "GET /users" $ assertResp
      (Client.request api.getUsers {})
      [{ id: 1, name: "John Doe" }]
    test "GET /users/<id>" $ assertResp
      (Client.request api.getUser { id: 1 })
      { id: 1, name: "John Doe" }
    test "GET /users/profile" $ assertResp
      (Client.request api.getUsersProfiles {})
      ["Profile1", "Profile2"]
    test "POST /users/new" $ assertResp
      (Client.request api.createUser { body: { id: 5, name: "New user!" }})
      { id: 5, name: "New user!" }
    test "GET /users/<id>/posts/<postId>" $ assertResp
      (Client.request api.getUserPost { id: 1, postId: "1" })
      { id: "1", text: "Some post" }
    test "GET /pages/<id>" $ assertResp
      (Client.request api.getPage { id: "1" })
      "Page 1"
    test "GET /pages/<id>/metadata" $ assertResp
      (Client.request api.getPageMetadata { id: "1"})
      "Page metadata 1"

startTestServer :: Aff Unit
startTestServer = do
  let opts = { hostname: "localhost", port: 3000, backlog: Nothing }
  startResult <- Payload.start opts api handlers
  case startResult of
    Right _ -> pure unit
    Left err -> liftEffect (log err)

tests :: TestSuite
tests = do
  UrlParsingTest.tests
  ParamsTest.tests
  RoutingTest.tests
  TrieTest.tests
  serverTests

main :: Effect Unit
main = Aff.launchAff_ $ do
  startTestServer
  liftEffect $ runTest tests
