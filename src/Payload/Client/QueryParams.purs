module Payload.Client.QueryParams
       ( class EncodeQueryParam
       , encodeQueryParam
       , class EncodeQueryParamMulti
       , encodeQueryParamMulti
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Global (encodeURIComponent)

class EncodeQueryParam a where
  encodeQueryParam :: a -> Maybe String

instance encodeQueryParamInt :: EncodeQueryParam Int where
  encodeQueryParam val = Just (show val)

instance encodeQueryParamString :: EncodeQueryParam String where
  encodeQueryParam s = encodeURIComponent s

instance encodeQueryParamMaybe :: EncodeQueryParam a => EncodeQueryParam (Maybe a) where
  encodeQueryParam (Just val) = encodeQueryParam val
  encodeQueryParam Nothing = Nothing

class EncodeQueryParamMulti a where
  encodeQueryParamMulti :: a -> Maybe String

instance encodeQueryParamMultiObjectArrayString :: EncodeQueryParamMulti (Object (Array String)) where
  encodeQueryParamMulti o = case sequence (encodeArray <$> Object.toUnfoldable o) of
    Just encodedArrays -> Just (String.joinWith "&" encodedArrays)
    Nothing -> Nothing
    where
      encodeArray :: Tuple String (Array String) -> Maybe String
      encodeArray (Tuple k vals) =
        case sequence (encodeVal k <$> vals) of
          Just encodedVals -> Just (String.joinWith "&" encodedVals)
          Nothing -> Nothing

      encodeVal :: String -> String -> Maybe String
      encodeVal key val = case Tuple (encodeURIComponent key) (encodeURIComponent val) of
        Tuple (Just encodedKey) (Just encodedVal) -> Just $ encodedKey <> "=" <> encodedVal
        _ -> Nothing
