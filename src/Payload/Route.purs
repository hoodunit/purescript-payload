module Payload.Route where

import Prelude
import Payload.Internal.GuardParsing (GNil, Guards(..))

data Route (m :: Symbol) (p :: Symbol) spec = Route
type GET = Route "GET"
type HEAD = Route "HEAD"
type POST = Route "POST"
type PUT = Route "PUT"
type DELETE = Route "DELETE"

type DefaultRequest =
  ( params :: {}
  , query :: {}
  , body :: String
  , guards :: Guards GNil
  , response :: Unit )
