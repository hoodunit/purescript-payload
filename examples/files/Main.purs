module Payload.Examples.Files.Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.String as String
import Effect.Aff (Aff)
import Payload.GuardParsing (GuardTypes(..))
import Payload.Handlers (File(..))
import Payload.Route (GET, Route(..))
import Payload.Test.Helpers (withServer)
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTestWith)
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

request :: String -> Aff (Either String String)
request path = do
  res <- AX.get ResponseFormat.string ("http://localhost:3000/" <> path)
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

runTests :: Aff Unit
runTests = withServer api { handlers, guards: {} } (runTestWith Fancy.runTest tests)
