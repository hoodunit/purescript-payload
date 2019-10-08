module Payload.Params
       ( DecodeError
       , class FromParam
       , class FromSegments
       , class ToParam
       , fromParam
       , fromSegments
       , toParam
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.List (List)
import Data.Maybe (maybe)

type DecodeError = String

class FromParam a where
  fromParam :: String -> Either DecodeError a

instance fromParamInt :: FromParam Int where
  fromParam s = maybe (Left errorMsg) Right (Int.fromString s)
    where
      errorMsg = "Could not decode '" <> s <> "' into an Int"

instance fromParamString :: FromParam String where
  fromParam s = Right s

class FromSegments a where
  fromSegments :: List String -> Either DecodeError a

instance fromSegmentsListString :: FromSegments (List String) where
  fromSegments s = Right s

class ToParam a where
  toParam :: a -> String

instance toParamInt :: ToParam Int where
  toParam = show

instance toParamString :: ToParam String where
  toParam s = s
