module Payload.Client.Options where

import Payload.Headers (Headers)

type Options =
  { baseUrl :: String
  , logLevel :: LogLevel }

data LogLevel = LogNormal | LogDebug

type RequestOptions =
  { headers :: Headers }

defaultOpts :: Options
defaultOpts =
  { baseUrl: ""
  , logLevel: LogNormal
  }
