module Payload.Examples.Files.Test where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Payload.Examples.Files.Main (api, spec)
import Payload.Test.Helpers (respMatches, withServer)
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert

assertResp :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertResp req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors
  
tests :: TestSuite
tests = do
  let get = Helpers.get "http://localhost:3000"
  let withApi = withServer spec api
  suite "Example: file serving" do
    suite "single file" do
      test "serves single file" $ withApi do
        res <- get "/"
        respMatches { status: 200, body: "<html lang=\"en\"></html>\n" } res
      test "sets content-type" $ withApi do
        res <- get "/"
        Assert.equal (Just "text/html") (Map.lookup "content-type" res.headers)
    suite "directory" do
      test "serves file" $ withApi do
        res <- get "/test.json"
        respMatches { status: 200, body: "{ \"foo\": \"bar\" }\n"} res
      test "serves nested file" $ withApi do
        res <- get "/css/styles.css"
        respMatches { status: 200, body: ".app {}\n"} res
      test "returns 404 for non-existent file" $ withApi do
        res <- get "/nonexistent.json"
        Assert.equal 404 res.status
      test "returns 404 for trailing slash" $ withApi do
        res <- get "/nonexistent.json/"
        Assert.equal 404 res.status
      test "returns 404 for directory" $ withApi do
        res <- get "/css"
        Assert.equal 404 res.status
      test "does not return file outside of directory" $ withApi do
        res <- get "../private.json"
        Assert.equal 404 res.status
      test "handles URL-encoded paths" $ withApi do
        res <- get "/another%20test.json"
        respMatches { status: 200, body: "{ \"foo\": \"bar\" }\n"} res
      test "serves empty file" $ withApi do
        res <- get "/empty"
        respMatches { status: 200, body: ""} res
