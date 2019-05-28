module Payload.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Payload.Examples.Basic.Main as BasicExample
import Payload.Examples.Files.Main as FilesExample
import Payload.Test.Cookies as CookiesTest
import Payload.Test.GuardParsing as GuardParsingTest
import Payload.Test.Params as ParamsTest
import Payload.Test.Routing as RoutingTest
import Payload.Test.Trie as TrieTest
import Payload.Test.UrlParsing as UrlParsingTest
import Test.Unit (TestSuite)
import Test.Unit.Main (runTest)

tests :: TestSuite
tests = do
  UrlParsingTest.tests
  ParamsTest.tests
  RoutingTest.tests
  TrieTest.tests
  GuardParsingTest.tests
  CookiesTest.tests

main :: Effect Unit
main = Aff.launchAff_ $ do
  liftEffect $ runTest tests
  BasicExample.runTests
  FilesExample.runTests
