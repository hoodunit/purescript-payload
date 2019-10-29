module Payload.Examples.Hello.Test where

import Prelude

import Payload.Examples.Hello.Main (handlers, spec)
import Payload.Test.Helpers (respMatches, withRoutes)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  let withApi = withRoutes spec handlers
  suite "Hello world" do
    let { get } = Helpers.request "http://localhost:3000"
    test "GET /user/12/messages?limit=2 succeeds" $ withApi do
      res <- get "/users/12/messages?limit=2"
      respMatches
        { status: 200
        , body: "[{\"text\":\"Hey there\",\"id\":1},{\"text\":\"Limit 2\",\"id\":2}]" }
        res
    test "GET /user/12/messages?limit=2.5 fails with 404 Not Found" $ withApi do
      res <- get "/users/12/messages?limit=2.5"
      Assert.equal 404 res.status
