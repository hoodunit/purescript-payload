module Payload.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Payload.Examples.Basic.Test as BasicExample
import Payload.Examples.Files.Test as FilesExample
import Payload.Examples.Hello.Test as HelloExample
import Payload.Examples.Movies.Test as MoviesExample
import Payload.Examples.Transformer.Test as TransformerExample
import Payload.Test.Config (defaultConfig)
import Payload.Test.Integration.Client.Errors as ClientErrorsTest
import Payload.Test.Integration.Client.Methods as ClientMethodsTest
import Payload.Test.Integration.Client.Options as ClientOptionsTest
import Payload.Test.Integration.Client.QueryParams as ClientQueryParams
import Payload.Test.Integration.Client.Statuses as ClientStatuses
import Payload.Test.Integration.Client.ContentTypes as ClientContentTypes
import Payload.Test.Integration.Server.Body as BodyTest
import Payload.Test.Integration.Server.Guards as GuardsTest
import Payload.Test.Integration.Server.Methods as MethodsTest
import Payload.Test.Integration.Server.QueryParams as QueryParamsTest
import Payload.Test.Integration.Server.Status as StatusTest
import Payload.Test.Integration.Server.Routing as RoutingTest
import Payload.Test.Unit.Client.QueryParams as ClientQueryParamsTest
import Payload.Test.Unit.Client.EncodeParam as ClientEncodeParamTest
import Payload.Test.Unit.Internal.QueryParsing as QueryParsingTest
import Payload.Test.Unit.Internal.UrlParsing as UrlParsingTest
import Payload.Test.Unit.Server.Cookies as CookiesTest
import Payload.Test.Unit.Server.Internal.OmitEmpty as OmitEmptyTest
import Payload.Test.Unit.Server.Internal.Query as QueryTest
import Payload.Test.Unit.Server.Internal.Trie as TrieTest
import Payload.Test.Unit.Server.Internal.Url as UrlTest
import Payload.Test.Unit.Server.Params as ParamsTest
import Payload.Test.Unit.Server.Response as ResponseTest
import Test.Unit (TestSuite, suite)
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy
  
tests :: TestSuite
tests = do
  let cfg = defaultConfig
  suite "Unit - Shared" do
    UrlParsingTest.tests
    QueryParsingTest.tests

  suite "Unit - Server" do
    CookiesTest.tests
    OmitEmptyTest.tests
    ParamsTest.tests
    QueryTest.tests
    ResponseTest.tests
    TrieTest.tests
    UrlTest.tests

  suite "Unit - Client" do
    ClientQueryParamsTest.tests
    ClientEncodeParamTest.tests

  suite "Integration - Server" do
    BodyTest.tests
    MethodsTest.tests
    QueryParamsTest.tests
    GuardsTest.tests
    StatusTest.tests
    RoutingTest.tests

  suite "Integration - Client" do
    ClientErrorsTest.tests cfg
    ClientMethodsTest.tests cfg
    ClientOptionsTest.tests cfg
    ClientQueryParams.tests cfg
    ClientStatuses.tests cfg
    ClientContentTypes.tests cfg

  suite "Examples" do
    HelloExample.tests
    BasicExample.tests cfg
    FilesExample.tests
    MoviesExample.tests cfg
    TransformerExample.tests

main :: Effect Unit
main = Aff.launchAff_ $ do
  runTestWith Fancy.runTest tests
