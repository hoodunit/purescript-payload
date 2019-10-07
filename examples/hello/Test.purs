module Payload.Examples.Hello.Test where

import Prelude

import Effect.Class (liftEffect)
import Payload.Examples.Hello.Main (api, main, spec)
import Payload.Server as Payload
import Payload.Test.Helpers (loggingErrors, respMatches, withServer)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  suite "Hello world" do
    let { get } = Helpers.request "http://localhost:3000"
    test "GET /user/12/messages?limit=2 succeeds" $ do
      loggingErrors (Payload.start_ spec api) do
        res <- get "/users/12/messages?limit=2"
        respMatches
          { status: 200
          , body: "[{\"text\":\"Hey there\",\"id\":1},{\"text\":\"Limit 2\",\"id\":2}]" }
          res
    test "GET /user/12/messages?limit=2.5 fails with 404 Not Found" $ do
      loggingErrors (Payload.start_ spec api) do
        res <- get "/users/12/messages?limit=2.5"
        Assert.equal 404 res.status
