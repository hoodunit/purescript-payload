module Payload.Test.Integration.Error where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Payload.Headers as Headers
import Payload.Response (Failure, RawResponse, Response(..), ResponseBody(..))
import Payload.Response as Response
import Payload.Spec (GET, Spec(Spec))
import Payload.Status as Status
import Payload.Test.Helpers (respMatches, withRoutes)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
  
tests :: TestSuite
tests = do
  let { get } = Helpers.request "http://localhost:3000"
  suite "Errors" do
    test "to return a String as an internal error, return Either String" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ -> pure (Left "Error!" :: Either String String) }
      withRoutes spec handlers do
        res <- get "/foo"
        respMatches { status: 500, body: "Error" } res
    test "to return an arbitrary error response, return Either ServerError" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ -> pure (Left (Response.Error $ Response.badRequest (StringBody "Error!")) :: Either Failure String) }
      withRoutes spec handlers do
        res <- get "/foo"
        Assert.equal { status: 400, body: "Error!", headers: Map.empty } res
