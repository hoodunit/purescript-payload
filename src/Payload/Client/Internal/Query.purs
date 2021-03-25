module Payload.Client.Internal.Query where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String as String
import Payload.Client.QueryParams (class EncodeQueryParam, class EncodeQueryParamMulti, encodeQueryParam, encodeQueryParamMulti)
import Payload.Internal.QueryParsing (Key, Multi, class ParseQuery, QueryCons, QueryListProxy(..), QueryNil, QueryList)
import Prim.Row as Row
import Record as Record
import Type.Prelude (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

class EncodeQuery (urlStr :: Symbol) (query :: Row Type) | urlStr -> query where
  encodeQuery :: Proxy urlStr
                 -> Record query
                 -> String

instance encodeQuerySymbol ::
  ( ParseQuery urlStr queryParts
  , EncodeQueryList queryParts query
  ) => EncodeQuery urlStr query where
 encodeQuery _ query = case encoded of
   Nil -> ""
   e -> "?" <> String.joinWith "&" (Array.fromFoldable e)
   where
     encoded = encodeQueryList (QueryListProxy :: _ queryParts) query

class EncodeQueryList
      (queryParts :: QueryList)
      (query :: Row Type) where
  encodeQueryList :: QueryListProxy queryParts
                -> Record query
                -> List String

instance encodeQueryListNil :: EncodeQueryList QueryNil query where
  encodeQueryList _ _ = Nil

instance encodeQueryListConsKey ::
  ( IsSymbol queryKey
  , IsSymbol ourKey
  , Row.Cons ourKey valType queryRest query
  , Row.Lacks ourKey queryRest
  , EncodeQueryParam valType
  , EncodeQueryList rest queryRest
  ) => EncodeQueryList
         (QueryCons (Key queryKey ourKey) rest)
         query where
  encodeQueryList _ query =
    case encodeQueryParam val of
      Just encoded -> (label <> "=" <> encoded) : rest
      Nothing -> rest
    where
      label = reflectSymbol (Proxy :: Proxy queryKey) 
      val = Record.get (Proxy :: Proxy ourKey) query
      queryRest = Record.delete (Proxy :: Proxy ourKey) query
      rest = encodeQueryList (QueryListProxy :: _ rest) queryRest

instance encodeQueryListConsMulti ::
  ( IsSymbol ourKey
  , Row.Cons ourKey valType () query
  , EncodeQueryParamMulti valType
  ) => EncodeQueryList (QueryCons (Multi ourKey) QueryNil) query where
  encodeQueryList _ query = case encodeQueryParamMulti queryObj of
    Just encoded -> encoded : Nil
    Nothing -> Nil
    where
      queryObj = Record.get (Proxy :: _ ourKey) query
