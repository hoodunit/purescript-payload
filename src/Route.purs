module Payload.Route where

import Data.Symbol
import Prelude

import Payload.GuardParsing (GNil, Guards(..))

data Route (m :: Symbol) (p :: Symbol) spec = Route
data GET_ (m :: Symbol) spec = GET_
data HEAD_ (m :: Symbol) spec = HEAD_
data POST_ (m :: Symbol) spec = POST_
data PUT_ (m :: Symbol) spec = PUT_
data DELETE_ (m :: Symbol) spec = DELETE_
type GET = Route "GET"
type HEAD = Route "HEAD"
type POST = Route "POST"
type PUT = Route "PUT"
type DELETE = Route "DELETE"

type DefaultRequest =
  ( params :: {}
  , query :: {}
  , body :: {}
  , guards :: Guards GNil )
defaultSpec :: Record DefaultRequest
defaultSpec =
  { params: {}
  , query: {}
  , body: {}
  , guards: Guards :: _ GNil
  }
