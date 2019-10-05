module Payload.Test.Integration.Guards where

import Prelude

import Data.Either (Either(..))
import Data.String.Utils as StringUtils
import Effect.Aff (Aff)
import Node.HTTP as HTTP
import Payload.Response as Resp
import Payload.Spec (type (:), API(API), GET, Guards(..), Nil)
import Payload.Test.Helpers (withServer)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy

newtype User = User
  { id :: Int
  , name :: String }

newtype AdminUser = AdminUser
  { id :: Int
  , name :: String }

api :: API
  { guards :: { user :: User, adminUser :: AdminUser }
  , routes ::
    { adminIndex :: GET "/admin"
        { guards :: Guards ("adminUser" : Nil)
        , response :: String }
    , userIndex :: GET "/user"
        { guards :: Guards ("user" : Nil)
        , response :: String }
    , unauthenticatedIndex :: GET "/"
        { response :: String }}}
api = API

adminIndex :: forall r. { | r} -> Aff String
adminIndex _ = pure "Admin page"

userIndex :: forall r. { | r} -> Aff String
userIndex _ = pure "User page"

unauthenticatedIndex :: forall r. { | r} -> Aff String
unauthenticatedIndex _ = pure "Unauthenticated page"

getAdminUser :: HTTP.Request -> Aff (Either Resp.Failure AdminUser)
getAdminUser req = do
  if StringUtils.endsWith "secret" (HTTP.requestURL req)
     then pure (Right (AdminUser { id: 1, name: "John Admin" }))
     else pure (Left (Resp.Forward "Not an admin"))

getUser :: HTTP.Request -> Aff (Either Resp.Failure User)
getUser req = do
  if StringUtils.endsWith "username" (HTTP.requestURL req)
     then pure (Right (User { id: 1, name: "John User" }))
     else pure (Left (Resp.Forward "Not a user"))
  
tests :: TestSuite
tests = do
  suite "Guards" do
    let { get } = Helpers.request "http://localhost:3000"
    test "GET /admin succeeds if secret is provided" $ do
      res <- get "/admin?secret"
      Assert.equal "Admin page" res.body
    test "GET /admin fails with 404 if no secret is given" $ do
      res <- get "/admin"
      Assert.equal 404 res.status
    test "GET /user succeeds if username is provided" $ do
      res <- get "/user?username"
      Assert.equal "User page" res.body
    test "GET /user fails with 404 if no username is provided" $ do
      res <- get "/user"
      Assert.equal 404 res.status
    test "GET / succeeds" $ do
      res <- get "/"
      Assert.equal "Unauthenticated page" res.body

runTests :: Aff Unit
runTests = do
  let guards = { adminUser: getAdminUser, user: getUser }
  let handlers = { adminIndex, userIndex, unauthenticatedIndex }
  withServer api { guards, handlers } (runTestWith Fancy.runTest tests)
