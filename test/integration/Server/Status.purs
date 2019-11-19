module Payload.Test.Integration.Server.Status where

import Prelude

import Effect.Aff (Aff)
import Payload.ResponseTypes (Response)
import Payload.Server.Response as Response
import Payload.Spec (GET, Spec(Spec))
import Payload.Test.Helpers (respMatches, withRoutes)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
  
tests :: TestSuite
tests = do
  let { get } = Helpers.request "http://localhost:3000"
  suite "Status" do
    test "to modify the status, return a Response with a new status" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: { id :: Int } } }
      let (foo :: forall r. { | r } -> Aff (Response { id :: Int }) ) = \_ -> do
            pure $ Response.created { id: 1 }
      let handlers = { foo }
      withRoutes spec handlers do
        res <- get "/foo"
        respMatches { status: 201, body: "{\"id\":1}" } res
