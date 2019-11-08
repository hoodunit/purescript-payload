module Payload.Internal.Route
       ( DefaultRouteSpec
       , DefaultRouteSpecNoBody
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

type DefaultRouteSpecNoBody =
  ( params :: {}
  , query :: {}
  , body :: Undefined
  , guards :: Guards GNil
  , response :: Response.Empty )

data Undefined
