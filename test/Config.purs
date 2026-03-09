module Payload.Test.Config where

import Prelude

import Payload.Client as Client

testPort :: Int
testPort = 38123

baseUrl :: String
baseUrl = "http://localhost:" <> show testPort

type TestConfig =
  { clientOpts :: Client.Options }

defaultConfig :: TestConfig
defaultConfig =
  { clientOpts: Client.defaultOpts { baseUrl = baseUrl } }
