module Payload.Server.QueryParams
       ( class DecodeQueryParam
       , decodeQueryParam
       , class DecodeQueryParamMulti
       , decodeQueryParamMulti
       , DecodeError(QueryDecodeError, QueryParamNotFound)
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Foreign.Object (Object)
import Foreign.Object as Object

data DecodeError
  = QueryDecodeError {key :: String, strValue :: String, message :: String, queryObj :: Object String}
  | QueryParamNotFound {key :: String, queryObj :: Object String}

instance showDecodeError :: Show DecodeError where
  show (QueryDecodeError e) = "(QueryDecodeError " <> show e <> ")"
  show (QueryParamNotFound e) = "(QueryParamNotFound " <> show e <> ")"

class DecodeQueryParam a where
  decodeQueryParam :: Object String -> String -> Either DecodeError a

instance decodeQueryParamInt :: DecodeQueryParam Int where
  decodeQueryParam queryObj queryKey =
    case Object.lookup queryKey queryObj of
      Nothing -> Left (QueryParamNotFound {key: queryKey, queryObj})
      Just str -> maybe (Left (decodeErr str)) Right (Int.fromString str)
    where
      decodeErr str = QueryDecodeError {key: queryKey, strValue: str, message: "Could not decode into an Int", queryObj}

instance decodeQueryParamString :: DecodeQueryParam String where
  decodeQueryParam queryObj queryKey =
    case Object.lookup queryKey queryObj of
      Nothing -> Left (QueryParamNotFound {key: queryKey, queryObj})
      Just str -> Right str

instance decodeQueryParamMaybe :: DecodeQueryParam a => DecodeQueryParam (Maybe a) where
  decodeQueryParam queryObj queryKey =
    case Object.lookup queryKey queryObj of
      Nothing -> Right Nothing
      Just _ -> Just <$> decodeQueryParam queryObj queryKey

class DecodeQueryParamMulti a where
  decodeQueryParamMulti :: Object String -> Either DecodeError a

instance decodeQueryParamMultiObjectString :: DecodeQueryParamMulti (Object String) where
  decodeQueryParamMulti o = Right o
