module Payload.Test.Integration.Error where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Payload.Headers as Headers
import Payload.Response (RawResponse, Response(..), ResponseBody(..), ServerError(..), serverError)
import Payload.Route (GET)
import Payload.Status as Status
import Payload.Test.Helpers (respMatches, withRoutes)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))
  
tests :: TestSuite
tests = do
  let { get } = Helpers.request "http://localhost:3000"
  suite "Errors" do
    test "to return a String as an internal error, return Either String" $ do
      let spec = Proxy :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ -> pure (Left "Error!" :: Either String String) }
      withRoutes spec handlers do
        res <- get "/foo"
        respMatches { status: 500, body: "Error" } res
    test "to return a specific error status, return ServerError" $ do
      let spec = Proxy :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ -> pure (Left (serverError Status.badRequest "Error!") :: Either ServerError String)}
      withRoutes spec handlers do
        res <- get "/foo"
        Assert.equal { status: 400, body: "Error!", headers: Map.empty } res
    test "to return an arbitrary error response, return Error RawResponse" $ do
      let spec = Proxy :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ -> pure (Left (Response { status: Status.badRequest, headers: Headers.empty, body: StringBody "Error!" }) :: Either RawResponse String) }
      withRoutes spec handlers do
        res <- get "/foo"
        Assert.equal { status: 400, body: "Error!", headers: Map.empty } res
