module Payload.QueryParams where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.List (List)
import Data.Maybe (maybe)

type DecodeError = String

class FromQueryParam a where
  fromQueryParam :: String -> Either DecodeError a

instance fromQueryParamInt :: FromQueryParam Int where
  fromQueryParam s = maybe (Left errorMsg) Right (Int.fromString s)
    where
      errorMsg = "Could not decode '" <> s <> "' into an Int"

instance fromQueryParamString :: FromQueryParam String where
  fromQueryParam s = Right s

class ToQueryParam a where
  toParam :: a -> String

instance toQueryParamInt :: ToQueryParam Int where
  toParam = show

instance toQueryParamString :: ToQueryParam String where
  toParam s = s
