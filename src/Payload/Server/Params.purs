module Payload.Server.Params
       ( DecodeError
       , class DecodeParam
       , decodeParam
       , class DecodeSegments
       , decodeSegments
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.List (List)
import Data.Maybe (maybe)

type DecodeError = String

class DecodeParam a where
  decodeParam :: String -> Either DecodeError a

instance decodeParamInt :: DecodeParam Int where
  decodeParam s = maybe (Left errorMsg) Right (Int.fromString s)
    where
      errorMsg = "Could not decode '" <> s <> "' into an Int"

instance decodeParamString :: DecodeParam String where
  decodeParam s = Right s

class DecodeSegments a where
  decodeSegments :: List String -> Either DecodeError a

instance decodeSegmentsListString :: DecodeSegments (List String) where
  decodeSegments s = Right s
