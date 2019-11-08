module Payload.QueryParams
       ( class DecodeQueryParam
       , decodeQueryParam
       , class DecodeQueryParamMulti
       , decodeQueryParamMulti
       , class EncodeQueryParam
       , encodeQueryParam
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (maybe)
import Foreign.Object (Object)

type DecodeError = String

class DecodeQueryParam a where
  decodeQueryParam :: String -> Either DecodeError a

instance decodeQueryParamInt :: DecodeQueryParam Int where
  decodeQueryParam s = maybe (Left errorMsg) Right (Int.fromString s)
    where
      errorMsg = "Could not decode '" <> s <> "' into an Int"

instance decodeQueryParamString :: DecodeQueryParam String where
  decodeQueryParam s = Right s

class DecodeQueryParamMulti a where
  decodeQueryParamMulti :: Object String -> Either DecodeError a

instance decodeQueryParamMultiObjectString :: DecodeQueryParamMulti (Object String) where
  decodeQueryParamMulti o = Right o

class EncodeQueryParam a where
  encodeQueryParam :: a -> String

instance encodeQueryParamInt :: EncodeQueryParam Int where
  encodeQueryParam = show

instance encodeQueryParamString :: EncodeQueryParam String where
  encodeQueryParam s = s
