module Payload.Internal.Route
       ( DefaultRouteSpec
       , DefaultServerRouteSpec
       , DefaultParentRoute
       , Undefined(Undefined)
       ) where

import Data.Symbol (SProxy(..))
import Payload.ResponseTypes (Empty(..))
import Payload.Spec (Docs(..), Guards(Guards), Nil, Tags(Tags))

type DefaultServerRouteSpec =
  ( params :: {}
  , query :: {}
  , body :: Undefined
  , guards :: Guards Nil
  , response :: Empty
  , summary :: SProxy ""
  , description :: SProxy ""
  , tags :: Tags Nil)

type DefaultRouteSpec =
  ( params :: {}
  , query :: Undefined
  , body :: Undefined
  , guards :: Guards Nil
  , response :: Empty
  , summary :: SProxy ""
  , description :: SProxy ""
  , tags :: Tags Nil)

data Undefined = Undefined

type DefaultParentRoute =
  ( params :: {}
  , guards :: Guards Nil
  , docs :: Docs Undefined )
defaultParent :: Record DefaultParentRoute
defaultParent =
  { params: {}
  , guards: Guards
  , docs: Docs }
