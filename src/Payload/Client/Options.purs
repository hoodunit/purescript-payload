module Payload.Client.Options where

import Payload.Headers (Headers)
import Payload.Headers as Headers

type Options =
  { baseUrl :: String
  , logLevel :: LogLevel
  , extraHeaders :: Headers
  }

data LogLevel = LogNormal | LogDebug

type RequestOptions =
  { headers :: Headers }

defaultOpts :: Options
defaultOpts =
  { baseUrl: ""
  , logLevel: LogNormal
  , extraHeaders: Headers.empty
  }
