module Payload.Client.Internal.Url where

import Prelude

import Data.Array as Array
import Data.List (List)
import Data.String as String
import Payload.Client.Params (class ToParam, toParam)
import Payload.Internal.UrlParsing (class ParseUrl, UrlListProxy(..), Key, Lit, Multi, UrlCons, UrlNil, kind UrlList)
import Prim.Row as Row
import Record as Record
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)

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
