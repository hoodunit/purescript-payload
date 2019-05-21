module Payload.Examples.Files.Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Client as Client
import Payload.GuardParsing (GNil, GuardTypes(..), Guards(..))
import Payload.Handlers (File(..))
import Payload.Route (GET, Route(..), POST)
import Payload.Server as Payload
import Payload.Test.Params as ParamsTest
import Payload.Test.Routing as RoutingTest
import Payload.Test.Trie as TrieTest
import Payload.Test.UrlParsing as UrlParsingTest
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest, runTestWith)
import Test.Unit.Output.Fancy as Fancy

api =
  { guards: GuardTypes :: _ {}
  , routes:
    { indexPage: Route :: GET "/"
        { response :: File }
    , getAll: Route :: GET "/<..path>"
        { params :: { path :: List String }
        , response :: File }
    }
  }

handlers = { indexPage, getAll }

indexPage :: forall r. { | r} -> Aff File
indexPage _ = pure (File "examples/files/index.html")

-- Exposes to directory traversal attack
getAll :: forall r. { path :: List String | r} -> Aff File
getAll { path } = pure (File $ "examples/files/public/" <> String.joinWith "/" (Array.fromFoldable path))

request :: forall res. String -> Aff (Either String String)
request path = do
  res <- AX.get ResponseFormat.string ("http://localhost:3001/" <> path)
  let showingError = lmap ResponseFormat.printResponseFormatError
  pure $ showingError res.body

assertResp :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertResp req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors
  
tests :: TestSuite
tests = do
  suite "Example: file serving" do
    test "GET / (index.html)" $ do
      res <- request "/"
      Assert.equal (Right "<html lang=\"en\"></html>\n") res
    test "GET /test.json (from public dir)" $ do
      res <- request "/test.json"
      Assert.equal (Right "{ \"foo\": \"bar\" }\n") res

startTestServer :: Aff Unit
startTestServer = do
  let opts = Payload.defaultOpts { logLevel = Payload.LogError, port = 3001 }
  startResult <- Payload.start opts api { handlers, guards: {} }
  case startResult of
    Right _ -> pure unit
    Left err -> liftEffect (log err)

runTests :: Aff Unit
runTests = do
  startTestServer
  runTestWith (Fancy.runTest) tests
