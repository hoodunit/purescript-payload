module Payload.Test.Integration.QueryParams where

import Prelude

import Payload.Spec (GET, POST)
import Payload.Test.Helpers (respMatches, withRoutes)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
import Type.Proxy (Proxy(..))

tests :: TestSuite
tests = do
  let { get, post } = Helpers.request "http://localhost:3000"
  suite "Query parameter requests" do
    suite "keys (foo=<myFoo>)" do
      test "GET /search?limit=3 succeeds with valid int" $ do
        let spec = Proxy :: _ { search :: GET "/search?limit=<limit>"
                                { query :: { limit :: Int }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?limit=3"
          respMatches { status: 200, body: "Search result" } res
      test "GET /search?limit=3.1 fails with invalid integer" $ do
        let spec = Proxy :: _ { search :: GET "/search?limit=<limit>"
                                { query :: { limit :: Int }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?limit=3.1"
          respMatches { status: 404, body: "" } res
      test "GET /search?limit=asdf fails with non-integer" $ do
        let spec = Proxy :: _ { search :: GET "/search?limit=<limit>"
                                { query :: { limit :: Int }
                                , response :: String }}
        let handlers = { search: \_ -> pure $ "Search result" }
        withRoutes spec handlers do
          res <- get "/search?limit=asdf"
          respMatches { status: 404, body: "" } res
      test "POST /profile?id=3&foo=asdf succeeds" $ do
        let spec = Proxy :: _ { profile :: POST "/profile?id=<id>&foo=<foo>"
                                { query :: { id :: Int, foo :: String }
                                , body :: String
                                , response :: String }}
        let handlers = { profile: \(_ :: { id :: Int, foo :: String, body :: String }) -> pure "Saved profile" }
        withRoutes spec handlers do
          res <- post "/profile?id=3&foo=asdf" ""
          respMatches { status: 200, body: "Saved profile" } res
      test "POST /profile?id=3.0&foo=asdf fails" $ do
        let spec = Proxy :: _ { profile :: POST "/profile?id=<id>&foo=<foo>"
                                { query :: { id :: Int, foo :: String }
                                , body :: String
                                , response :: String }}
        let handlers = { profile: \(_ :: { id :: Int, foo :: String, body :: String }) -> pure "Saved profile" }
        withRoutes spec handlers do
          res <- post "/profile?id=3.0&foo=asdf" ""
          respMatches { status: 404, body: "" } res
