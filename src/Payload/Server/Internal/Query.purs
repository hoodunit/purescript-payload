module Payload.Server.Internal.Query where

import Prelude

import Data.Either (Either(..))
import Foreign.Object as Object
import Payload.Internal.QueryParsing (Key, Multi, class ParseQuery, QueryCons, QueryListProxy(..), QueryNil, QueryList)
import Payload.Server.Internal.Querystring (ParsedQuery, querystringParse)
import Payload.Server.QueryParams (class DecodeQueryParam, class DecodeQueryParamMulti, decodeQueryParam, decodeQueryParamMulti)
import Prim.Row as Row
import Record as Record
import Type.Equality (class TypeEquals, to)
import Type.Prelude (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

class DecodeQuery (queryUrlSpec :: Symbol) query | queryUrlSpec -> query where
  decodeQuery :: Proxy queryUrlSpec -> Proxy (Record query) -> String -> Either String (Record query)

instance decodeQueryAny ::
  ( ParseQuery queryUrlSpec queryParts
  , MatchQuery queryParts query () query
  ) => DecodeQuery queryUrlSpec query where
  decodeQuery _ queryType queryStr = matchQuery (QueryListProxy :: _ queryParts) queryType {} parsedQuery
    where
      parsedQuery = querystringParse queryStr

class MatchQuery (queryParts :: QueryList) query from to | queryParts -> from to where
  matchQuery :: QueryListProxy queryParts
                -> Proxy (Record query)
                -> Record from
                -> ParsedQuery
                -> Either String (Record to)

instance matchQueryNil ::
  ( TypeEquals (Record from) (Record to)
  ) => MatchQuery QueryNil query from to where
  matchQuery _ _ query _ = Right (to query)

instance matchQueryConsMulti ::
  ( IsSymbol ourKey
  , Row.Cons ourKey valType from from'
  , Row.Cons ourKey valType _params params
  , Row.Lacks ourKey from
  , DecodeQueryParamMulti valType
  , TypeEquals (Record from') (Record to)
  ) => MatchQuery (QueryCons (Multi ourKey) QueryNil) params from to where
  matchQuery _ queryType query queryObj =
    case decodeQueryParamMulti queryObj of
      Left errors -> Left $ show errors
      Right decoded -> Right (to $ Record.insert (Proxy :: Proxy ourKey) decoded query)

instance matchQueryConsKey ::
  ( IsSymbol queryKey
  , IsSymbol ourKey
  , MatchQuery rest params from' to
  , Row.Cons ourKey valType from from'
  , Row.Cons ourKey valType _params params
  , Row.Lacks ourKey from
  , DecodeQueryParam valType
  ) => MatchQuery (QueryCons (Key queryKey ourKey) rest) params from to where
  matchQuery _ queryType query queryObj =
    case decodeQueryParam queryObj queryKey of
      Left errors -> Left $ show errors
      Right decoded -> let newParams = Record.insert (Proxy :: Proxy ourKey) decoded query
                           newQueryObj = Object.delete queryKey queryObj
                       in matchQuery (QueryListProxy :: _ rest) queryType newParams newQueryObj
    where
      queryKey = reflectSymbol (Proxy :: Proxy queryKey)
