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
import Payload.Server.Internal.Querystring (ParsedQuery)

data DecodeError
  = QueryDecodeError {key :: String, values :: Array String, message :: String, queryObj :: ParsedQuery}
  | QueryParamNotFound {key :: String, queryObj :: ParsedQuery}

instance showDecodeError :: Show DecodeError where
  show (QueryDecodeError e) = "(QueryDecodeError " <> show e <> ")"
  show (QueryParamNotFound e) = "(QueryParamNotFound " <> show e <> ")"

class DecodeQueryParam a where
  decodeQueryParam :: ParsedQuery -> String -> Either DecodeError a

instance decodeQueryParamInt :: DecodeQueryParam Int where
  decodeQueryParam queryObj queryKey =
    case Object.lookup queryKey queryObj of
      Nothing -> Left (QueryParamNotFound {key: queryKey, queryObj})
      Just [] -> decodeErr [] $ "Expected single value but received empty array."
      Just [str] -> maybe (decodeErr [str] "Could not decode into an Int") Right (Int.fromString str)
      Just arr -> decodeErr arr $ "Expected single value but received multiple: " <> show arr
    where
      decodeErr values msg = Left (QueryDecodeError {key: queryKey, values, message: msg, queryObj})

instance decodeQueryParamString :: DecodeQueryParam String where
  decodeQueryParam queryObj queryKey =
    case Object.lookup queryKey queryObj of
      Nothing -> Left (QueryParamNotFound {key: queryKey, queryObj})
      Just [] -> decodeErr [] $ "Expected single value but received empty Array"
      Just [str] -> Right str
      Just arr -> decodeErr arr $ "Expected single value but received multiple: " <> show arr
    where
      decodeErr values msg = Left (QueryDecodeError {key: queryKey, values, message: msg, queryObj})

instance decodeQueryParamBoolean :: DecodeQueryParam Boolean where
  decodeQueryParam queryObj queryKey =
    case Object.lookup queryKey queryObj of
      Nothing -> Left (QueryParamNotFound {key: queryKey, queryObj})
      Just [] -> decodeErr [] $ "Expected single value but received empty Array"
      Just ["false"] -> Right false
      Just ["true"] -> Right true
      Just arr -> decodeErr arr $ "Expected single value but received multiple: " <> show arr
    where
      decodeErr values msg = Left (QueryDecodeError {key: queryKey, values, message: msg, queryObj})

instance decodeQueryParamMaybe :: DecodeQueryParam a => DecodeQueryParam (Maybe a) where
  decodeQueryParam queryObj queryKey =
    case Object.lookup queryKey queryObj of
      Nothing -> Right Nothing
      Just [] -> Right Nothing
      Just [""] -> Right Nothing
      Just _ -> Just <$> decodeQueryParam queryObj queryKey

class DecodeQueryParamMulti a where
  decodeQueryParamMulti :: ParsedQuery -> Either DecodeError a

instance decodeQueryParamMultiObjectString :: DecodeQueryParamMulti (Object (Array String)) where
  decodeQueryParamMulti o = Right o
