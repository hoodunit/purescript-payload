module Payload.Client.QueryParams
       ( class EncodeQueryParam
       , encodeQueryParam
       , class EncodeQueryParamMulti
       , encodeQueryParamMulti
       ) where

import Prelude

import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

class EncodeQueryParam a where
  encodeQueryParam :: a -> String

instance encodeQueryParamInt :: EncodeQueryParam Int where
  encodeQueryParam = show

instance encodeQueryParamString :: EncodeQueryParam String where
  encodeQueryParam s = s

class EncodeQueryParamMulti a where
  encodeQueryParamMulti :: a -> String

instance encodeQueryParamMultiObjectString :: EncodeQueryParamMulti (Object String) where
  encodeQueryParamMulti o = String.joinWith "&" (encodeEntry <$> Object.toUnfoldable o)
    where
      encodeEntry :: forall v. Show v => Tuple String v -> String
      encodeEntry (Tuple k v) = k <> "=" <> show v
