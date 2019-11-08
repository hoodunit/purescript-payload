module Payload.Client.Options where

import Affjax as AX

type Options =
  { baseUrl :: String }

type ModifyRequest = AX.Request String -> AX.Request String

defaultOpts :: Options
defaultOpts =
  { baseUrl: ""
  }
