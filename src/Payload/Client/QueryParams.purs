module Payload.Client.QueryParams
       ( class EncodeQueryParam
       , encodeQueryParam
       ) where

import Prelude

class EncodeQueryParam a where
  encodeQueryParam :: a -> String

instance encodeQueryParamInt :: EncodeQueryParam Int where
  encodeQueryParam = show

instance encodeQueryParamString :: EncodeQueryParam String where
  encodeQueryParam s = s
