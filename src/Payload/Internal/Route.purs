module Payload.Internal.Route
       ( DefaultRequest
       ) where

import Payload.Response as Response
import Payload.Spec (GNil, Guards)

type DefaultRequest =
  ( params :: {}
  , query :: {}
  , body :: String
  , guards :: Guards GNil
  , response :: Response.Empty )
