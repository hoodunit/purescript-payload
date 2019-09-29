module Payload.Url where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Payload.Params (class FromParam, class FromSegments, class ToParam, fromParam, fromSegments, toParam)
import Payload.UrlParsing (class ParseUrl, UrlListProxy(..), Key, Lit, Multi, UrlCons, UrlNil, kind UrlList)
import Prim.Row as Row
import Record as Record
import Type.Equality (class TypeEquals, to)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))

class EncodeUrl (urlStr :: Symbol) params | urlStr -> params where
  encodeUrl :: SProxy urlStr -> Record params -> String

instance encodeUrlRecord ::
  ( ParseUrl urlStr urlParts
  , WriteUrl urlParts params
  ) => EncodeUrl urlStr params where
  encodeUrl _ params = writeUrl (UrlListProxy :: _ urlParts) params

class WriteUrl (urlParts :: UrlList) params where
  writeUrl :: UrlListProxy urlParts -> Record params -> String

instance writeUrlUrlNil :: WriteUrl UrlNil params where
  writeUrl _ params = ""

instance writeUrlConsKey ::
  ( IsSymbol key
  , Row.Cons key valType from params
  , ToParam valType
  , WriteUrl rest params
  ) => WriteUrl (UrlCons (Key key) rest) params where
  writeUrl _ params = "/" <> encodedParam <> restOfUrl
    where
      encodedParam = toParam (Record.get (SProxy :: SProxy key) params)
      restOfUrl = writeUrl (UrlListProxy :: _ rest) params

instance writeUrlConsLit ::
  ( IsSymbol lit
  , WriteUrl rest params
  ) => WriteUrl (UrlCons (Lit lit) rest) params where
  writeUrl _ params = "/" <> litStr <> restOfUrl
    where
      litStr = reflectSymbol (SProxy :: SProxy lit)
      restOfUrl = writeUrl (UrlListProxy :: _ rest) params

instance writeUrlConsMulti ::
  ( IsSymbol multiKey
  , Row.Cons multiKey (List String) from params
  ) => WriteUrl (UrlCons (Multi multiKey) UrlNil) params where
  writeUrl _ params = "/" <> multiStr
    where
      multiStr = String.joinWith "/" (Array.fromFoldable $ Record.get (SProxy :: _ multiKey) params)

class DecodeUrl (urlStr :: Symbol) params | urlStr -> params where
  decodeUrl :: SProxy urlStr -> Proxy (Record params) -> List String -> Either String (Record params)

instance decodeUrlSymbol ::
  ( ParseUrl urlStr urlParts
  , MatchUrl urlParts params () params
  ) => DecodeUrl urlStr params where
  decodeUrl _ paramsType path = match (UrlListProxy :: _ urlParts) paramsType {} path

class MatchUrl (urlParts :: UrlList) params from to | urlParts -> from to where
  match :: UrlListProxy urlParts -> Proxy (Record params) -> Record from -> List String -> Either String (Record to)

instance matchUrlUrlNil ::
  ( TypeEquals (Record from) (Record to)
  ) => MatchUrl UrlNil params from to where
  match _ _ params Nil = Right (to params)
  match _ _ _ path = Left $ "Path mismatch: Ran out of params when path still had '" <> show path <> "'"

instance matchUrlMulti ::
  ( IsSymbol key
  , Row.Cons key valType from to
  , Row.Lacks key from
  , FromSegments valType
  ) => MatchUrl (UrlCons (Multi key) UrlNil) to from to where
  match _ paramsType params segments = case fromSegments segments of
    Left errors -> Left $ show errors
    Right decoded -> Right $ Record.insert (SProxy :: SProxy key) decoded params

instance matchUrlConsKey ::
  ( IsSymbol key
  , MatchUrl rest params from' to
  , Row.Cons key valType from from'
  , Row.Cons key valType _params params
  , Row.Lacks key from
  , FromParam valType
  ) => MatchUrl (UrlCons (Key key) rest) params from to where
  match _ paramsType params Nil = Left "Decoding error at key"
  match _ paramsType params (segment : rest) = case fromParam segment of
    Left errors -> Left $ show errors
    Right decoded -> let newParams = Record.insert (SProxy :: SProxy key) decoded params in
      match (UrlListProxy :: _ rest) paramsType newParams rest

instance matchUrlConsLit ::
  ( IsSymbol lit
  , MatchUrl rest params from to
  ) => MatchUrl (UrlCons (Lit lit) rest) params from to where
  match _ paramsType params Nil = Left "Decoding error at literal"
  match _ paramsType params (segment : rest) =
    match (UrlListProxy :: _ rest) paramsType params rest
