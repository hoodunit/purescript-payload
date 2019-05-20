module Payload.Route where

import Prelude

import Data.Symbol

data Route (m :: Symbol) (p :: Symbol) spec = Route
type GET = Route "GET"
type POST = Route "POST"

type DefaultRequest = ( params :: {}, body :: {}, guards :: {} )
defaultSpec :: Record DefaultRequest
defaultSpec =
  { params: {}
  , body: {}
  , guards: {}
  }
