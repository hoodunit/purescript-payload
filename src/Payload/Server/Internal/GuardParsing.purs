module Payload.Server.Internal.GuardParsing where

import Prelude

import Payload.Spec (GCons, GNil, GuardList)

foreign import data GuardParseFail :: GuardList

data GuardTypes :: forall k. k -> Type
data GuardTypes types = GuardTypes

class Append (left :: GuardList) (right :: GuardList) (both :: GuardList) | left right -> both

instance appendGuardLeftNil :: Append GNil right right
instance appendGuards ::
  ( Append rest (GCons s right) both
  ) => Append (GCons s rest) right both
