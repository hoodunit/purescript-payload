module Payload.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Payload.Examples.Basic.Main as BasicExample
import Payload.Examples.Files.Main as FilesExample
import Payload.Examples.Movies.Main as MoviesExample
import Payload.Test.Integration.Guards as GuardsTest
import Payload.Test.Integration.Methods as MethodsTest
import Payload.Test.Integration.QueryParams as QueryParamsTest
import Payload.Test.Unit.Cookies as CookiesTest
import Payload.Test.Unit.Internal.GuardParsing as GuardParsingTest
import Payload.Test.Unit.Internal.QueryParsing as QueryParsingTest
import Payload.Test.Unit.Internal.Trie as TrieTest
import Payload.Test.Unit.Internal.Url as UrlTest
import Payload.Test.Unit.Internal.UrlParsing as UrlParsingTest
import Payload.Test.Unit.Params as ParamsTest
import Payload.Test.Unit.Response as ResponseTest
import Test.Unit (TestSuite)
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy

tests :: TestSuite
tests = do
  UrlParsingTest.tests
  UrlTest.tests
  QueryParsingTest.tests
  ParamsTest.tests
  TrieTest.tests
  GuardParsingTest.tests
  CookiesTest.tests
  ResponseTest.tests
  MethodsTest.tests

main :: Effect Unit
main = Aff.launchAff_ $ do
  runTestWith Fancy.runTest tests
  GuardsTest.runTests
  QueryParamsTest.runTests
  FilesExample.runTests
  BasicExample.runTests
  MoviesExample.runTests
