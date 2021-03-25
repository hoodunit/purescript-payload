module Payload.Internal.Route
       ( DefaultRouteSpec
       , DefaultServerRouteSpec
       , DefaultParentRoute
       , Undefined(Undefined)
       ) where

import Payload.ResponseTypes (Empty)
import Payload.Spec (GNil, Guards(..))

type DefaultServerRouteSpec =
  ( params :: {}
  , query :: {}
  , body :: Undefined
  , guards :: Guards GNil
  , response :: Empty )

type DefaultRouteSpec =
  ( params :: {}
  , query :: Undefined
  , body :: Undefined
  , guards :: Guards GNil
  , response :: Empty )

data Undefined = Undefined

type DefaultParentRoute = ( params :: {}, guards :: Guards GNil )
defaultParent :: Record DefaultParentRoute
defaultParent =
  { params: {}, guards: Guards :: _ GNil }
