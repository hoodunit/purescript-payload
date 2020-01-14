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
import Payload.Client.Internal.EncodeUri (encodeUri)

class EncodeQueryParam a where
  encodeQueryParam :: a -> Maybe String

instance encodeQueryParamInt :: EncodeQueryParam Int where
  encodeQueryParam val = Just (show val)

instance encodeQueryParamString :: EncodeQueryParam String where
  encodeQueryParam s = Just (encodeUri s)

instance encodeQueryParamBoolean :: EncodeQueryParam Boolean where
  encodeQueryParam true = Just "true"
  encodeQueryParam false = Just "false"

instance encodeQueryParamMaybe :: EncodeQueryParam a => EncodeQueryParam (Maybe a) where
  encodeQueryParam (Just val) = encodeQueryParam val
  encodeQueryParam Nothing = Nothing

class EncodeQueryParamMulti a where
  encodeQueryParamMulti :: a -> Maybe String

instance encodeQueryParamMultiObjectArrayString :: EncodeQueryParamMulti (Object (Array String)) where
  encodeQueryParamMulti o = Just (joinParams (encodeArray <$> Object.toUnfoldable o))
    where
      joinParams :: Array String -> String
      joinParams = String.joinWith "&"
      
      encodeArray :: Tuple String (Array String) -> String
      encodeArray (Tuple k vals) = joinParams (encodeVal k <$> vals)

      encodeVal :: String -> String -> String
      encodeVal key val = encodeUri key <> "=" <> encodeUri val
