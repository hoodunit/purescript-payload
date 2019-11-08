module Payload.Client.Internal.Url where

import Prelude

import Data.Array as Array
import Data.List (List)
import Data.String as String
import Payload.Client.EncodeParam (class EncodeParam, encodeParam)
import Payload.Internal.UrlParsing (class ParseUrl, UrlListProxy(..), Key, Lit, Multi, UrlCons, UrlNil, kind UrlList)
import Prim.Row as Row
import Record as Record
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))

class EncodeUrl
      (urlStr :: Symbol)
      (params :: # Type)
      (payload :: # Type) | urlStr -> params where
  encodeUrl :: SProxy urlStr -> Proxy (Record params) -> Record payload -> String

instance encodeUrlRecord ::
  ( ParseUrl urlStr urlParts
  , WriteUrl urlParts params payload
  ) => EncodeUrl urlStr params payload where
  encodeUrl _ params payload =
    writeUrl (UrlListProxy :: _ urlParts) params payload

class WriteUrl
      (urlParts :: UrlList)
      (params :: # Type)
      (payload :: # Type) where
  writeUrl :: UrlListProxy urlParts -> Proxy (Record params) -> Record payload -> String

instance writeUrlUrlNil :: WriteUrl UrlNil params payload where
  writeUrl _ _ params = ""

instance writeUrlConsKey ::
  ( IsSymbol key
  , Row.Cons key valType from params
  , Row.Cons key valType from' payload
  , EncodeParam valType
  , WriteUrl rest params payload
  ) => WriteUrl (UrlCons (Key key) rest) params payload where
  writeUrl _ p payload = "/" <> encodedParam <> restOfUrl
    where
      encodedParam = encodeParam (Record.get (SProxy :: SProxy key) payload)
      restOfUrl = writeUrl (UrlListProxy :: _ rest) p payload

instance writeUrlConsLit ::
  ( IsSymbol lit
  , WriteUrl rest params payload
  ) => WriteUrl (UrlCons (Lit lit) rest) params payload where
  writeUrl _ p payload = "/" <> litStr <> restOfUrl
    where
      litStr = reflectSymbol (SProxy :: SProxy lit)
      restOfUrl = writeUrl (UrlListProxy :: _ rest) p payload

instance writeUrlConsMulti ::
  ( IsSymbol multiKey
  , Row.Cons multiKey (List String) from params
  , Row.Cons multiKey (List String) from' payload
  ) => WriteUrl (UrlCons (Multi multiKey) UrlNil) params payload where
  writeUrl _ _ payload = "/" <> multiStr
    where
      multiList = Record.get (SProxy :: _ multiKey) payload
      multiStr = String.joinWith "/" (Array.fromFoldable multiList)
