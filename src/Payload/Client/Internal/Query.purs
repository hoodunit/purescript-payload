module Payload.Client.Internal.Query where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Payload.Client.QueryParams (class EncodeQueryParam, class EncodeQueryParamMulti, encodeQueryParam, encodeQueryParamMulti)
import Payload.Internal.QueryParsing (kind QueryPart, Lit, Key, Multi, class ParseQuery, QueryCons, QueryListProxy(..), QueryNil, kind QueryList)
import Payload.Internal.Querystring.Qs as Qs
import Prim.Row as Row
import Record as Record
import Type.Equality (class TypeEquals, to)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))

class EncodeQuery (urlStr :: Symbol) query payload where
  encodeQuery :: SProxy urlStr
                 -> Proxy (Record query)
                 -> Record payload
                 -> String

instance encodeQuerySymbol ::
  ( ParseQuery queryUrlSpec queryParts
  , EncodeQueryList queryParts query payload
  ) => EncodeQuery queryUrlSpec query payload where
 encodeQuery _ q payload = case encoded of
   Nil -> ""
   e -> "?" <> String.joinWith "&" (Array.fromFoldable e)
   where
     encoded = encodeQueryList (QueryListProxy :: _ queryParts) q payload

class EncodeQueryList (queryParts :: QueryList) query payload where
  encodeQueryList :: QueryListProxy queryParts
                 -> Proxy (Record query)
                -> Record payload
                -> List String

instance encodeQueryListNil :: EncodeQueryList QueryNil query payload where
  encodeQueryList _ _ _ = Nil

instance encodeQueryListConsLiteral ::
  ( IsSymbol lit
  , EncodeQueryList rest query payload
  ) => EncodeQueryList (QueryCons (Lit lit) rest) query payload where
  encodeQueryList _ q payload = literal : rest
    where
      literal = reflectSymbol (SProxy :: SProxy lit)
      rest = encodeQueryList (QueryListProxy :: _ rest) q payload

instance encodeQueryListConsKey ::
  ( IsSymbol queryKey
  , IsSymbol ourKey
  , Row.Cons queryKey valType query' query
  , Row.Cons ourKey valType payload' payload
  , EncodeQueryParam valType
  , EncodeQueryList rest query' payload
  ) => EncodeQueryList (QueryCons (Key queryKey ourKey) rest) query payload where
  encodeQueryList _ q payload = queryKey : rest
    where
      label = reflectSymbol (SProxy :: SProxy queryKey) 
      val = Record.get (SProxy :: SProxy ourKey) payload
      encoded = encodeQueryParam val
      queryKey = label <> "=" <> encodeQueryParam val
      rest = encodeQueryList (QueryListProxy :: _ rest)
                             (Proxy :: _ (Record query'))
                             payload

instance encodeQueryListConsMulti ::
  ( IsSymbol ourKey
  , Row.Cons ourKey valType payload' payload
  , EncodeQueryParamMulti valType
  ) => EncodeQueryList (QueryCons (Multi ourKey) QueryNil) query payload where
  encodeQueryList _ q payload = encodeQueryParamMulti queryObj : Nil
    where
      queryObj = Record.get (SProxy :: _ ourKey) payload
