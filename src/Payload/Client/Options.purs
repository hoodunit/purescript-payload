module Payload.Client.Options where

import Affjax as AX

type Options =
  { hostname :: String
  , port :: Int }

type ModifyRequest = AX.Request String -> AX.Request String

defaultOpts :: Options
defaultOpts =
  { hostname: "localhost"
  , port: 3000
  }
