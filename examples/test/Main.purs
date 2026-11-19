module Payload.Examples.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Payload.Examples.Basic.Test as BasicExample
import Payload.Examples.Files.Test as FilesExample
import Payload.Examples.Hello.Test as HelloExample
import Payload.Examples.Movies.Test as MoviesExample
import Payload.Examples.Test.Config (defaultConfig)
import Test.Unit (TestSuite, suite)
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy

tests :: TestSuite
tests = do
  let cfg = defaultConfig
  suite "Examples" do
    HelloExample.tests
    BasicExample.tests cfg
    FilesExample.tests
    MoviesExample.tests cfg

main :: Effect Unit
main = Aff.launchAff_ $ do
  runTestWith Fancy.runTest tests
