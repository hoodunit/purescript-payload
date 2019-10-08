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
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy

assertResp :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertResp req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors
  
tests :: TestSuite
tests = do
  let get = Helpers.get "http://localhost:3000"
  suite "Example: file serving" do
    suite "single file" do
      test "serves single file" $ do
        res <- get "/"
        respMatches { status: 200, body: "<html lang=\"en\"></html>\n" } res
      test "sets content-type" $ do
        res <- get "/"
        Assert.equal (Just "text/html") (Map.lookup "content-type" res.headers)
    suite "directory" do
      test "serves file" $ do
        res <- get "/test.json"
        respMatches { status: 200, body: "{ \"foo\": \"bar\" }\n"} res
      test "serves nested file" $ do
        res <- get "/css/styles.css"
        respMatches { status: 200, body: ".app {}\n"} res
      test "returns 404 for non-existent file" $ do
        res <- get "/nonexistent.json"
        Assert.equal 404 res.status
      test "returns 404 for trailing slash" $ do
        res <- get "/nonexistent.json/"
        Assert.equal 404 res.status
      test "returns 404 for directory" $ do
        res <- get "/css"
        Assert.equal 404 res.status
      test "does not return file outside of directory" $ do
        res <- get "../private.json"
        Assert.equal 404 res.status
      test "handles URL-encoded paths" $ do
        res <- get "/another%20test.json"
        respMatches { status: 200, body: "{ \"foo\": \"bar\" }\n"} res
      test "serves empty file" $ do
        res <- get "/empty"
        respMatches { status: 200, body: ""} res

runTests :: Aff Unit
runTests = withServer spec api (runTestWith Fancy.runTest tests)
