module Payload.Internal.Route
       ( DefaultRouteSpec
       , DefaultServerRouteSpec
       , DefaultParentRoute
       , Undefined(Undefined)
       ) where

import Data.Symbol (SProxy(..))
import Payload.ResponseTypes (Empty(..))
import Payload.Spec (GNil, Guards(..))

type DefaultServerRouteSpec =
  ( params :: {}
  , query :: {}
  , body :: Undefined
  , guards :: Guards GNil
  , response :: Empty
  , summary :: SProxy ""
  , description :: SProxy "")

type DefaultRouteSpec =
  ( params :: {}
  , query :: Undefined
  , body :: Undefined
  , guards :: Guards GNil
  , response :: Empty
  , summary :: SProxy ""
  , description :: SProxy "")

data Undefined = Undefined

type DefaultParentRoute = ( params :: {}, guards :: Guards GNil )
defaultParent :: Record DefaultParentRoute
defaultParent =
  { params: {}, guards: Guards :: _ GNil }
