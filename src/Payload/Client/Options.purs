module Payload.Client.Options where

import Payload.Headers (Headers)

type Options =
  { baseUrl :: String }

type RequestOptions =
  { headers :: Headers }

defaultOpts :: Options
defaultOpts =
  { baseUrl: ""
  }
