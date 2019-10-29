module Payload.QueryParams
       ( class FromQueryParam
       , fromQueryParam
       , class FromQueryParamMulti
       , fromQueryParamMulti
       , class ToQueryParam
       , toQueryParam
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (maybe)
import Foreign.Object (Object)

type DecodeError = String

class FromQueryParam a where
  fromQueryParam :: String -> Either DecodeError a

instance fromQueryParamInt :: FromQueryParam Int where
  fromQueryParam s = maybe (Left errorMsg) Right (Int.fromString s)
    where
      errorMsg = "Could not decode '" <> s <> "' into an Int"

instance fromQueryParamString :: FromQueryParam String where
  fromQueryParam s = Right s

class FromQueryParamMulti a where
  fromQueryParamMulti :: Object String -> Either DecodeError a

instance fromQueryParamMultiObjectString :: FromQueryParamMulti (Object String) where
  fromQueryParamMulti o = Right o

class ToQueryParam a where
  toQueryParam :: a -> String

instance toQueryParamInt :: ToQueryParam Int where
  toQueryParam = show

instance toQueryParamString :: ToQueryParam String where
  toQueryParam s = s
