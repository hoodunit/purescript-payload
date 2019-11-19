module Payload.Test.Integration.Server.QueryParams where

import Prelude

import Data.List (List)
import Foreign.Object (Object)
import Payload.Spec (GET, POST, Spec(..))
import Payload.Test.Helpers (respMatches, withRoutes)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  let { get, post } = Helpers.request "http://localhost:3000"
  suite "Query parameter requests" do
    suite "literals (e.g. foo)" do
      test "GET /search?foo succeeds when 'foo' is provided" $ do
        let spec = Spec :: _ { search :: GET "/search?foo"
                                { response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?foo"
          respMatches { status: 200, body: "Search result" } res
      test "GET /search?foo fails with 404 when 'foo' is not provided" $ do
        let spec = Spec :: _ { search :: GET "/search?foo"
                                { response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search"
          Assert.equal 404 res.status
      test "GET /search?foo fails with 404 when 'foo' is provided but not as a literal (foo=asdf)" $ do
        let spec = Spec :: _ { search :: GET "/search?foo"
                                { response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?foo=asdf"
          Assert.equal 404 res.status
      test "GET /search?foo succeeds when 'foo' is provided with empty string" $ do
        let spec = Spec :: _ { search :: GET "/search?foo"
                                { response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?foo="
          respMatches { status: 200, body: "Search result" } res
      test "GET /search?a=<a>&foo=asdf&b=<b> succeeds when 'foo' is first parameter" $ do
        let spec = Spec :: _ { search :: GET "/search?a=<a>&foo&b=<b>"
                                { query :: { a :: Int, b :: Int }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?foo&a=1&b=1"
          respMatches { status: 200, body: "Search result" } res
      test "GET /search?a=<a>&foo&b=<b> succeeds when 'foo' is last parameter" $ do
        let spec = Spec :: _ { search :: GET "/search?a=<a>&foo&b=<b>"
                                { query :: { a :: Int, b :: Int }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?a=1&b=1&foo"
          respMatches { status: 200, body: "Search result" } res


    suite "keys (e.g. foo=<myFoo>)" do
      test "GET /search?limit=3 succeeds with valid int" $ do
        let spec = Spec :: _ { search :: GET "/search?limit=<limit>"
                                { query :: { limit :: Int }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?limit=3"
          respMatches { status: 200, body: "Search result" } res
      test "GET /search?limit=3.1 fails with invalid integer" $ do
        let spec = Spec :: _ { search :: GET "/search?limit=<limit>"
                                { query :: { limit :: Int }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?limit=3.1"
          respMatches { status: 404, body: "" } res
      test "GET /search?limit=asdf fails with non-integer" $ do
        let spec = Spec :: _ { search :: GET "/search?limit=<limit>"
                                { query :: { limit :: Int }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?limit=asdf"
          respMatches { status: 404, body: "" } res
      test "POST /profile?id=3&foo=asdf succeeds" $ do
        let spec = Spec :: _ { profile :: POST "/profile?id=<id>&foo=<foo>"
                                { query :: { id :: Int, foo :: String }
                                , body :: String
                                , response :: String }}
        let handlers = { profile: \_ -> pure "Saved profile" }
        withRoutes spec handlers do
          res <- post "/profile?id=3&foo=asdf" ""
          respMatches { status: 200, body: "Saved profile" } res
      test "POST /profile?id=3&foo=asdf succeeds when id and foo are given in another order" $ do
        let spec = Spec :: _ { profile :: POST "/profile?id=<id>&foo=<foo>"
                                { query :: { id :: Int, foo :: String }
                                , body :: String
                                , response :: String }}
        let handlers = { profile: \_ -> pure "Saved profile" }
        withRoutes spec handlers do
          res <- post "/profile?foo=asdf&id=3" ""
          respMatches { status: 200, body: "Saved profile" } res
      test "POST /profile?id=3.0&foo=asdf fails" $ do
        let spec = Spec :: _ { profile :: POST "/profile?id=<id>&foo=<foo>"
                                { query :: { id :: Int, foo :: String }
                                , body :: String
                                , response :: String }}
        let handlers = { profile: \_ -> pure "Saved profile" }
        withRoutes spec handlers do
          res <- post "/profile?id=3.0&foo=asdf" ""
          respMatches { status: 404, body: "" } res


    suite "multi-match (e.g. foo=<myFoo>)" do
      test "GET /search?<..all> multimatch grabs all query parameters" $ do
        let spec = Spec :: _ { search :: GET "/search?<..all>"
                                { query :: { all :: Object String }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?limit=3"
          respMatches { status: 200, body: "Search result" } res
