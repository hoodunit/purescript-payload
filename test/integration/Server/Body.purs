module Payload.Test.Integration.Server.Body where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Foreign.Object (Object)
import Payload.Spec (GET, POST, PUT, Spec(..), DELETE)
import Payload.Test.Helpers (respMatches, withRoutes)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)

tests :: TestSuite
tests = do
  let { delete, post, put } = Helpers.request "http://localhost:3000"
  suite "Body" do
    suite "decoding" do
      test "sending a valid POST body returns valid 200 response" $ do
        let spec = Spec :: _ { foo :: POST "/foo" { body :: { id :: Int }, response :: String } }
        let handlers = { foo: \{body} -> pure (show body.id) }
        withRoutes spec handlers do
            res <- post "/foo" "{\"id\": 5}"
            respMatches { status: 200, body: "5" } res
      test "sending an invalid POST body returns 400 Bad Request" $ do
        let spec = Spec :: _ { foo :: POST "/foo" { body :: { id :: Int }, response :: String } }
        let handlers = { foo: \{body} -> pure (show body) }
        withRoutes spec handlers do
            res <- post "/foo" "asdf"
            respMatches { status: 400, body: "" } res
      test "sending a valid PUT body returns valid 200 response" $ do
        let spec = Spec :: _ { foo :: PUT "/foo" { body :: { id :: Int }, response :: String } }
        let handlers = { foo: \{body} -> pure (show body.id) }
        withRoutes spec handlers do
            res <- put "/foo" "{\"id\": 5}"
            respMatches { status: 200, body: "5" } res
      test "sending an invalid PUT body returns 400 Bad Request" $ do
        let spec = Spec :: _ { foo :: PUT "/foo" { body :: { id :: Int }, response :: String } }
        let handlers = { foo: \{body} -> pure (show body) }
        withRoutes spec handlers do
            res <- put "/foo" "asdf"
            respMatches { status: 400, body: "" } res
      test "sending a valid DELETE body returns valid 200 response" $ do
        let spec = Spec :: _ { foo :: DELETE "/foo" { body :: { id :: Int }, response :: String } }
        let handlers = { foo: \{body} -> pure (show body.id) }
        withRoutes spec handlers do
            res <- delete "/foo" (Just "{\"id\": 5}")
            respMatches { status: 200, body: "5" } res
      test "sending an invalid DELETE body returns 400 Bad Request" $ do
        let spec = Spec :: _ { foo :: DELETE "/foo" { body :: { id :: Int }, response :: String } }
        let handlers = { foo: \{body} -> pure (show body) }
        withRoutes spec handlers do
            res <- delete "/foo" (Just "asdf")
            respMatches { status: 400, body: "" } res
      test "sending valid body for optional body returns 200 response" $ do
        let spec = Spec :: _ { foo :: POST "/foo"
                               { body :: Maybe { id :: Int }, response :: String } }
        let handlers = { foo: \{body} -> pure $ maybe "No body" (_.id >>> show) body }
        withRoutes spec handlers do
            res <- post "/foo" "{\"id\": 5}"
            respMatches { status: 200, body: "5" } res
      test "sending no body for optional body returns 200 response" $ do
        let spec = Spec :: _ { foo :: POST "/foo"
                               { body :: Maybe { id :: Int }, response :: String } }
        let handlers = { foo: \{body} -> pure $ maybe "No body" (_.id >>> show) body }
        withRoutes spec handlers do
            res <- post "/foo" ""
            respMatches { status: 200, body: "No body" } res
