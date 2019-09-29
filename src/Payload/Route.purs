module Payload.Route where

import Prelude

import Payload.Spec (GNil, Guards(..))

type DefaultRequest =
  ( params :: {}
  , query :: {}
  , body :: String
  , guards :: Guards GNil
  , response :: Unit )
