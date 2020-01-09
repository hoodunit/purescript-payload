module Payload.Client.QueryParams
       ( class EncodeQueryParam
       , encodeQueryParam
       , class EncodeQueryParamMulti
       , encodeQueryParamMulti
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

class EncodeQueryParam a where
  encodeQueryParam :: a -> Maybe String

instance encodeQueryParamInt :: EncodeQueryParam Int where
  encodeQueryParam val = Just (show val)

instance encodeQueryParamString :: EncodeQueryParam String where
  encodeQueryParam s = Just s

instance encodeQueryParamMaybe :: EncodeQueryParam a => EncodeQueryParam (Maybe a) where
  encodeQueryParam (Just val) = encodeQueryParam val
  encodeQueryParam Nothing = Nothing

class EncodeQueryParamMulti a where
  encodeQueryParamMulti :: a -> Maybe String

instance encodeQueryParamMultiObjectString :: EncodeQueryParamMulti (Object String) where
  encodeQueryParamMulti o = Just (String.joinWith "&" (encodeEntry <$> Object.toUnfoldable o))
    where
      encodeEntry :: forall v. Show v => Tuple String v -> String
      encodeEntry (Tuple k v) = k <> "=" <> show v
