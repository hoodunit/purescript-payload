module Payload.Test.Integration.Server.Routing where

import Prelude

import Data.Foldable (intercalate)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Payload.Spec (GET, POST, PUT, Spec(Spec), DELETE)
import Payload.Test.Config as Config
import Payload.Test.Helpers (respMatches, withRoutes)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  let { get, post, put, delete } = Helpers.request Config.baseUrl
  suite "Routing" do
    suite "unusual request targets" do
      test "a request target that is not a URL is routed as a path and the server keeps serving" $ do
        let spec = Spec :: _ { foo :: GET "/foo" { response :: String } }
        let handlers = { foo: \_ -> pure "foo" }
        withRoutes spec handlers do
          res <- Helpers.rawGet "//["
          Assert.equal "HTTP/1.1 404 Not Found" res.status
          ok <- get "/foo"
          respMatches { status: 200, body: "foo" } ok
      test "a path starting with // is routed without its leading slashes" $ do
        let spec = Spec :: _ { user :: GET "/users/<id>" { params :: { id :: Int }, response :: String } }
        let handlers = { user: \{ params: { id } } -> pure ("user " <> show id) }
        withRoutes spec handlers do
          res <- Helpers.rawGet "//users/1"
          Assert.equal { status: "HTTP/1.1 200 OK", body: "user 1" } res
      test "dot segments are routed literally" $ do
        let spec = Spec :: _ { any :: GET "/<..rest>" { params :: { rest :: List String }, response :: String } }
        let handlers = { any: \{ params: { rest } } -> pure (intercalate "|" rest) }
        withRoutes spec handlers do
          dots <- Helpers.rawGet "/a/x/../b"
          Assert.equal "a|x|..|b" dots.body
          encodedDots <- Helpers.rawGet "/%2e%2e/b"
          Assert.equal "..|b" encodedDots.body
    suite "URL params" do
      test "sending valid URL params returns 200 response" $ do
        let spec = Spec :: _ { foo :: GET "/foo/<id>" { params :: { id :: Int }, response :: String }
                             , foo2 :: GET "/foo/<..any>" { params :: { any :: List String }, response :: String } }
        let handlers = { foo: \{params: {id}} -> pure "foo"
                       , foo2: \{params: {any}} -> pure "foo2" }
        withRoutes spec handlers do
            res <- get "/foo/12"
            respMatches { status: 200, body: "foo" } res
      test "sending invalid URL params forwards to next matching handler" $ do
        let spec = Spec :: _ { foo :: GET "/foo/<id>" { params :: { id :: Int }, response :: String }
                             , foo2 :: GET "/foo/<..any>" { params :: { any :: List String }, response :: String } }
        let handlers = { foo: \{params: {id}} -> pure "foo"
                       , foo2: \{params: {any}} -> pure "foo2" }
        withRoutes spec handlers do
            res <- get "/foo/asdf"
            respMatches { status: 200, body: "foo2" } res
    suite "query params" do
      test "sending valid query params returns 200 response" $ do
        let spec = Spec :: _ { foo :: GET "/foo?id=<id>"
                               { query :: { id :: Int }
                               , response :: String } }
        let handlers = { foo: \{query: {id}} -> pure (show id)}
        withRoutes spec handlers do
            res <- get "/foo?id=2"
            respMatches { status: 200, body: "2" } res
      test "sending invalid query params returns 400 Bad Request" $ do
        let spec = Spec :: _ { foo :: GET "/foo?id=<id>"
                               { query :: { id :: Int }
                               , response :: String } }
        let handlers = { foo: \{query: {id}} -> pure (show id) }
        withRoutes spec handlers do
            res <- get "/foo?id=asdf"
            respMatches { status: 400, body: "" } res
