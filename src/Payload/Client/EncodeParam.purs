module Payload.Client.EncodeParam
       ( class EncodeParam
       , encodeParam
       ) where

import Prelude

class EncodeParam a where
  encodeParam :: a -> String

instance encodeParamInt :: EncodeParam Int where
  encodeParam = show

instance encodeParamString :: EncodeParam String where
  encodeParam s = s
