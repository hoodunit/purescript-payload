module Payload.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Payload.Examples.Basic.Test as BasicExample
import Payload.Examples.Files.Test as FilesExample
import Payload.Examples.Hello.Test as HelloExample
import Payload.Examples.Movies.Test as MoviesExample
import Payload.Test.Config (defaultConfig)
import Payload.Test.Integration.Client.Methods as ClientMethodsTest
import Payload.Test.Integration.Client.QueryParams as ClientQueryParams
import Payload.Test.Integration.Guards as GuardsTest
import Payload.Test.Integration.Methods as MethodsTest
import Payload.Test.Integration.QueryParams as QueryParamsTest
import Payload.Test.Integration.Status as StatusTest
import Payload.Test.Unit.Cookies as CookiesTest
import Payload.Test.Unit.Internal.GuardParsing as GuardParsingTest
import Payload.Test.Unit.Internal.Query as QueryTest
import Payload.Test.Unit.Internal.QueryParsing as QueryParsingTest
import Payload.Test.Unit.Internal.Trie as TrieTest
import Payload.Test.Unit.Internal.Url as UrlTest
import Payload.Test.Unit.Internal.UrlParsing as UrlParsingTest
import Payload.Test.Unit.Params as ParamsTest
import Payload.Test.Unit.Response as ResponseTest
import Test.Unit (TestSuite, suite)
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy
  
tests :: TestSuite
tests = do
  let cfg = defaultConfig
  suite "Unit" do
    UrlParsingTest.tests
    UrlTest.tests
    QueryTest.tests
    QueryParsingTest.tests
    ParamsTest.tests
    TrieTest.tests
    GuardParsingTest.tests
    CookiesTest.tests
    ResponseTest.tests

  suite "Server integration" do
    MethodsTest.tests
    QueryParamsTest.tests
    GuardsTest.tests
    StatusTest.tests

  suite "Client integration" do
    ClientMethodsTest.tests cfg
    ClientQueryParams.tests cfg

  suite "Examples" do
    HelloExample.tests
    BasicExample.tests cfg
    FilesExample.tests
    MoviesExample.tests cfg

main :: Effect Unit
main = Aff.launchAff_ $ do
  runTestWith Fancy.runTest tests
