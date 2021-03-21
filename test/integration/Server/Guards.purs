module Payload.Test.Integration.Server.Guards where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Utils as StringUtils
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Node.HTTP as HTTP
import Payload.Headers as Headers
import Payload.ResponseTypes (Failure(..))
import Payload.Server.Guards as Guards
import Payload.Spec (type (:), GET, Guards, Nil, Routes, Spec(Spec))
import Payload.Test.Helpers (get_, respMatches, withServer)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

spec :: Spec
  { guards ::
    { user :: User
    , adminUser :: AdminUser }
  , routes ::
    { adminIndex :: GET "/admin"
        { guards :: Guards ("adminUser" : Nil)
        , response :: String }
    , userIndex :: GET "/user"
        { guards :: Guards ("user" : Nil)
        , response :: String }
    , unauthenticatedIndex :: GET "/"
        { response :: String }}}
spec = Spec

newtype User = User
  { id :: Int
  , name :: String }

newtype AdminUser = AdminUser
  { id :: Int
  , name :: String }

adminIndex :: forall r. { | r} -> Aff String
adminIndex _ = pure "Admin page"

userIndex :: forall r. { | r} -> Aff String
userIndex _ = pure "User page"

unauthenticatedIndex :: forall r. { | r} -> Aff String
unauthenticatedIndex _ = pure "Unauthenticated page"

getAdminUser :: HTTP.Request -> Aff (Either Failure AdminUser)
getAdminUser req = do
  headers <- Guards.headers req
  case Headers.lookup "Authorization" headers of
    (Just "Token secret") -> pure (Right (AdminUser { id: 1, name: "John Admin" }))
    _ -> pure (Left (Forward "Not an admin"))

getUser :: HTTP.Request -> Aff (Either Failure User)
getUser req = do
  if StringUtils.endsWith "username" (HTTP.requestURL req)
     then pure (Right (User { id: 1, name: "John User" }))
     else pure (Left (Forward "Not a user"))
  
tests :: TestSuite
tests = do
  let guards = { adminUser: getAdminUser, user: getUser }
  let handlers = { adminIndex, userIndex, unauthenticatedIndex }
  let api = { guards, handlers }
  suite "Guards" do
    let { get } = Helpers.request "http://localhost:3000"
    test "routes to guarded handler if guard is passed (secret is provied)" $ do
      withServer spec api do
        res <- get_ "http://localhost:3000" "/admin" (Headers.fromFoldable [Tuple "Authorization" "Token secret"])
        respMatches { status: 200, body: "Admin page" } res
    test "fails with 404 if guard is failed (no secret provided)" $ do
      withServer spec api do
        res <- get "/admin"
        Assert.equal 404 res.status
    test "routes to guarded handler if guard is passed (username is provided)" $ do
      withServer spec api do
        res <- get "/user?username"
        respMatches { status: 200, body: "User page" } res
    test "fails with 404 if guard is not passed (no username is provided)" $ do
      withServer spec api do
        res <- get "/user"
        Assert.equal 404 res.status
    test "unguarded call succeeds" $ do
      withServer spec api do
        res <- get "/"
        respMatches { status: 200, body: "Unauthenticated page" } res
    test "top-level guard: request fails if guard fails" $ do
      let spec_ = Spec :: Spec {
            guards :: {
               adminUser :: AdminUser
            },
            routes :: {
              guards :: Guards ("adminUser" : Nil),
              adminIndex :: GET "/admin" {
                 response :: String
              }
            }
          }
      withServer spec_ { guards: { adminUser: getAdminUser }, handlers: { adminIndex } } do
        res <- get "/admin"
        Assert.equal 404 res.status
    test "top-level guard: request succeeds if guard succeeds" $ do
      let spec_ = Spec :: Spec {
            guards :: {
               adminUser :: AdminUser
            },
            routes :: {
              guards :: Guards ("adminUser" : Nil),
              adminIndex :: GET "/admin" {
                 response :: String
              }
            }
          }
      withServer spec_ { guards: { adminUser: getAdminUser }, handlers: { adminIndex } } do
        res <- get_ "http://localhost:3000" "/admin" (Headers.fromFoldable [Tuple "Authorization" "Token secret"])
        Assert.equal 200 res.status
    test "parent routes guard: request fails if guard fails" $ do
      let spec_ = Spec :: Spec {
            guards :: {
               adminUser :: AdminUser
            },
            routes :: {
              all :: Routes "/all" {
                guards :: Guards ("adminUser" : Nil),
                adminIndex :: GET "/admin" {
                   response :: String
                }
              }
            }
          }
      withServer spec_ { guards: { adminUser: getAdminUser }, handlers: { all: { adminIndex } } } do
        res <- get "/admin"
        Assert.equal 404 res.status
    test "parent routes guard: request succeeds if guard succeeds" $ do
      let spec_ = Spec :: Spec {
            guards :: {
               adminUser :: AdminUser
            },
            routes :: {
              all :: Routes "/all" {
                guards :: Guards ("adminUser" : Nil),
                adminIndex :: GET "/admin" {
                   response :: String
                }
              }
            }
          }
      withServer spec_ { guards: { adminUser: getAdminUser }, handlers: { all: { adminIndex } } } do
        res <- get_ "http://localhost:3000" "/all/admin" (Headers.fromFoldable [Tuple "Authorization" "Token secret"])
        Assert.equal 200 res.status
    test "endpoint guard: request fails if guard fails" $ do
      let spec_ = Spec :: Spec {
            guards :: {
               adminUser :: AdminUser
            },
            routes :: {
              adminIndex :: GET "/admin" {
                 guards :: Guards ("adminUser" : Nil),
                 response :: String
              }
            }
          }
      withServer spec_ { guards: { adminUser: getAdminUser }, handlers: { adminIndex } } do
        res <- get "/admin"
        Assert.equal 404 res.status
    test "endpoint guard: request succeeds if guard succeeds" $ do
      let spec_ = Spec :: Spec {
            guards :: {
               adminUser :: AdminUser
            },
            routes :: {
              adminIndex :: GET "/admin" {
                 guards :: Guards ("adminUser" : Nil),
                 response :: String
              }
            }
          }
      withServer spec_ { guards: { adminUser: getAdminUser }, handlers: { adminIndex } } do
        res <- get_ "http://localhost:3000" "/admin" (Headers.fromFoldable [Tuple "Authorization" "Token secret"])
        Assert.equal 200 res.status
