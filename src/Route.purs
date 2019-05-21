module Payload.Route where

import Data.Symbol
import Prelude

import Payload.GuardParsing (GNil, Guards(..))

data Route (m :: Symbol) (p :: Symbol) spec = Route
type GET = Route "GET"
type POST = Route "POST"

type DefaultRequest = ( params :: {}, body :: {}, guards :: Guards GNil )
defaultSpec :: Record DefaultRequest
defaultSpec =
  { params: {}
  , body: {}
  , guards: Guards :: _ GNil
  }
