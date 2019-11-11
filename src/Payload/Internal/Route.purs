module Payload.Internal.Route
       ( DefaultRouteSpec
       , DefaultServerRouteSpec
       , Undefined
       ) where

import Payload.Response as Response
import Payload.Spec (GNil, Guards)

type DefaultServerRouteSpec =
  ( params :: {}
  , query :: {}
  , body :: String
  , guards :: Guards GNil
  , response :: Response.Empty )

type DefaultRouteSpec =
  ( params :: {}
  , query :: Undefined
  , body :: Undefined
  , guards :: Guards GNil
  , response :: Response.Empty )

data Undefined
