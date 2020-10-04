module Payload.Test.Integration.Server.QueryParams where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Payload.Spec (GET, POST, Spec(..))
import Payload.Test.Helpers (respMatches, withRoutes)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)

tests :: TestSuite
tests = do
  let { get, post } = Helpers.request "http://localhost:3000"
  suite "Query parameter requests" do
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
          respMatches { status: 400, body: "" } res
      test "GET /search?limit=asdf fails with non-integer" $ do
        let spec = Spec :: _ { search :: GET "/search?limit=<limit>"
                                { query :: { limit :: Int }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?limit=asdf"
          respMatches { status: 400, body: "" } res
      test "GET /search fails with missing required limit key" $ do
        let spec = Spec :: _ { search :: GET "/search?limit=<limit>"
                                { query :: { limit :: Int }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search"
          respMatches { status: 400, body: "" } res
      test "GET /search?limit=asdf succeeds with provided optional limit key" $ do
        let spec = Spec :: _ { search :: GET "/search?limit=<limit>"
                                { query :: { limit :: Maybe Int }
                                , response :: String }}
        let handlers = { search: \{query: {limit}} -> case limit of
                                                        Just l -> pure $ "Limit " <> show l
                                                        Nothing -> pure "No limit" }
        withRoutes spec handlers do
          res <- get "/search?limit=20"
          respMatches { status: 200, body: "Limit 20" } res
      test "GET /search?limit=asdf succeeds with missing optional limit key" $ do
        let spec = Spec :: _ { search :: GET "/search?limit=<limit>"
                                { query :: { limit :: Maybe Int }
                                , response :: String }}
        let handlers = { search: \{query: {limit}} -> case limit of
                                                        Just l -> pure $ "Limit " <> show l
                                                        Nothing -> pure "No limit" }
        withRoutes spec handlers do
          res <- get "/search"
          respMatches { status: 200, body: "No limit" } res
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
          respMatches { status: 400, body: "" } res
      test "GET /search?query=asdf succeeds with some String" $ do
        let spec = Spec :: _ { search :: GET "/search?query=<searchQuery>"
                                { query :: { searchQuery :: String }
                                , response :: String }}
        let handlers = { search: \{query: {searchQuery}} -> pure searchQuery }
        withRoutes spec handlers do
          res <- get "/search?query=asdf"
          respMatches { status: 200, body: "asdf" } res
      test "GET /search?query= succeeds with empty String" $ do
        let spec = Spec :: _ { search :: GET "/search?query=<searchQuery>"
                                { query :: { searchQuery :: String }
                                , response :: String }}
        let handlers = { search: \{query: {searchQuery}} -> pure searchQuery }
        withRoutes spec handlers do
          res <- get "/search?query="
          respMatches { status: 200, body: "" } res
      test "GET /search?query succeeds with missing String" $ do
        let spec = Spec :: _ { search :: GET "/search?query=<searchQuery>"
                                { query :: { searchQuery :: String }
                                , response :: String }}
        let handlers = { search: \{query: {searchQuery}} -> pure searchQuery }
        withRoutes spec handlers do
          res <- get "/search?query"
          respMatches { status: 200, body: "" } res


    suite "multi-match (e.g. foo=<myFoo>)" do
      test "GET /search?<..all> multimatch grabs all query parameters" $ do
        let spec = Spec :: _ { search :: GET "/search?<..all>"
                                { query :: { all :: Object (Array String) }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?limit=3"
          respMatches { status: 200, body: "Search result" } res

    suite "other checks" do
      test "GET /search?a=asdf&b=asdf&c=asdf ignores extra parameters (a and c)" $ do
        let spec = Spec :: _ { search :: GET "/search?b=<b>"
                                { query :: { b :: String }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?a=asdf&b=asdf&c=asdf"
          respMatches { status: 200, body: "Search result" } res
      test "GET /search?a=foo1&a=foo2 fails if given two duplicate params where one was specified" $ do
        let spec = Spec :: _ { search :: GET "/search?a=<a>"
                                { query :: { a :: String }
                                , response :: String }}
        let handlers = { search: \{query: {a}} -> pure a }
        withRoutes spec handlers do
          res <- get "/search?a=foo1&a=foo2"
          respMatches { status: 400, body: "" } res
