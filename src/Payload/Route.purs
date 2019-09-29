module Payload.Route where

import Prelude
import Payload.Internal.GuardParsing (GNil, Guards(..))

type DefaultRequest =
  ( params :: {}
  , query :: {}
  , body :: String
  , guards :: Guards GNil
  , response :: Unit )
