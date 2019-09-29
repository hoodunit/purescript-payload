module Payload.Test.Integration.Guards where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isLeft)
import Data.String.Utils as StringUtils
import Effect.Aff (Aff)
import Node.HTTP as HTTP
import Payload.Guards (GuardFn)
import Payload.Spec (type (:), API(API), GET, Guards(..), Nil)
import Payload.Test.Helpers (withServer)
import Test.Unit (TestSuite, Test, failure, suite, test)
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

getAdminUser :: GuardFn AdminUser
getAdminUser req = do
  if StringUtils.endsWith "secret" (HTTP.requestURL req)
     then pure (Right (AdminUser { id: 1, name: "John Admin" }))
     else pure (Left "Fail not an admin")

getUser :: GuardFn User
getUser req = do
  if StringUtils.endsWith "username" (HTTP.requestURL req)
     then pure (Right (User { id: 1, name: "John User" }))
     else pure (Left "Fail not a user")

request :: String -> Aff (Either String String)
request path = do
  res <- AX.get ResponseFormat.string ("http://localhost:3000/" <> path)
  let showingError = lmap ResponseFormat.printResponseFormatError
  if res.status == StatusCode 200
    then do
      pure $ showingError res.body
    else
      pure (Left $ "Received status code " <> show res.status <> "\n Body:\n" <> show (showingError res.body))

assertResp :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertResp req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors
  
tests :: TestSuite
tests = do
  suite "Guards" do
    test "GET /admin succeeds if secret is provided" $ do
      res <- request "/admin?secret"
      Assert.equal (Right "Admin page") res
    test "GET /admin fails if no secret is given" $ do
      res <- request "/admin"
      Assert.assert "Expected failure" (isLeft res)
    test "GET /user succeeds if username is provided" $ do
      res <- request "/user?username"
      Assert.equal (Right "User page") res
    test "GET /user fails if no username is provided" $ do
      res <- request "/user"
      Assert.assert "Expected failure" (isLeft res)
    test "GET / succeeds" $ do
      res <- request "/"
      Assert.equal (Right "Unauthenticated page") res

runTests :: Aff Unit
runTests = do
  let guards = { adminUser: getAdminUser, user: getUser }
  let handlers = { adminIndex, userIndex, unauthenticatedIndex }
  withServer api { guards, handlers } (runTestWith Fancy.runTest tests)
