module Payload.Test.Integration.Server.Error where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Payload.ResponseTypes (RawResponse, Response, ResponseBody(..))
import Payload.Server.Response as Response
import Payload.Spec (GET, Spec(Spec))
import Payload.Test.Helpers (respMatches, withRoutes)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  let { get } = Helpers.request "http://localhost:3000"
  suite "Errors" do
    test "returning Either String returns a String as an internal error" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ -> pure (Left "Error!" :: Either String String) }
      withRoutes spec handlers do
        res <- get "/foo"
        respMatches { status: 500, body: "Error" } res
    test "returning Either String (Response String) returns a String as an internal error" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ -> pure (Left "Error!" :: Either String (Response String)) }
      withRoutes spec handlers do
        res <- get "/foo"
        respMatches { status: 500, body: "Error" } res
    test "returning Either Array returns JSON as an internal error" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ -> pure (Left [1, 2] :: Either (Array Int) String) }
      withRoutes spec handlers do
        res <- get "/foo"
        respMatches { status: 500, body: "Error" } res
    test "returning Either Array (Response String) returns JSON as an internal error" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ -> pure (Left [1, 2] :: Either (Array Int) (Response String)) }
      withRoutes spec handlers do
        res <- get "/foo"
        respMatches { status: 500, body: "Error" } res
    test "returning Either (Response Record) String returns JSON" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ ->
                        pure (Left (Response.badRequest { id: 1 }) :: Either (Response { id :: Int }) String) }
      withRoutes spec handlers do
        res <- get "/foo"
        Assert.equal { status: 400, body: "{\"id\": 1}", headers: Map.empty } res
    test "returning Either (Response Record) (Response String) returns JSON" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ ->
                        pure (Left (Response.badRequest { id: 1 }) :: Either (Response { id :: Int }) (Response String)) }
      withRoutes spec handlers do
        res <- get "/foo"
        Assert.equal { status: 400, body: "{\"id\": 1}", headers: Map.empty } res
    test "to return an arbitrary error response, return Either ServerError" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ -> pure (Left (Response.badRequest (StringBody "Error!")) :: Either RawResponse String) }
      withRoutes spec handlers do
        res <- get "/foo"
        Assert.equal { status: 400, body: "Error!", headers: Map.empty } res
