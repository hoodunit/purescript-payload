module Payload.Test.Config where

import Payload.Client as Client

type TestConfig =
  { clientOpts :: Client.Options }

defaultConfig :: TestConfig
defaultConfig =
  { clientOpts: Client.defaultOpts { baseUrl = "http://localhost:3000" } }
