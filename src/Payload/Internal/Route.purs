module Payload.Internal.Route
       ( DefaultRouteSpec
       , DefaultRouteSpecOptionalBody
       , Undefined
       ) where

import Payload.Response as Response
import Payload.Spec (GNil, Guards)

type DefaultRouteSpec =
  ( params :: {}
  , query :: {}
  , body :: String
  , guards :: Guards GNil
  , response :: Response.Empty )

type DefaultRouteSpecOptionalBody =
  ( params :: {}
  , query :: {}
  , body :: Undefined
  , guards :: Guards GNil
  , response :: Response.Empty )

data Undefined
