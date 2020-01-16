module Payload.Client.Options where

import Payload.Headers (Headers)
import Payload.Headers as Headers

type Options =
  { baseUrl :: String
  , logLevel :: LogLevel
  , extraHeaders :: Headers
  }

data LogLevel = LogNormal | LogDebug

defaultOpts :: Options
defaultOpts =
  { baseUrl: ""
  , logLevel: LogNormal
  , extraHeaders: Headers.empty
  }

type RequestOptions =
  { extraHeaders :: Headers }

defaultReqOpts :: RequestOptions
defaultReqOpts =
  { extraHeaders: Headers.empty }
