module Payload.Client.Params
       ( class ToParam
       , toParam
       ) where

import Prelude

type DecodeError = String

class ToParam a where
  toParam :: a -> String

instance toParamInt :: ToParam Int where
  toParam = show

instance toParamString :: ToParam String where
  toParam s = s
