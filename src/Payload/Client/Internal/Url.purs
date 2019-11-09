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
      | urlStr -> params where
  encodeUrl :: SProxy urlStr -> Record params -> String

instance encodeUrlRecord ::
  ( ParseUrl urlStr urlParts
  , WriteUrl urlParts params
  ) => EncodeUrl urlStr params where
  encodeUrl _ params = writeUrl (UrlListProxy :: _ urlParts) params

class WriteUrl
      (urlParts :: UrlList)
      (params :: # Type) where
  writeUrl :: UrlListProxy urlParts -> Record params -> String

instance writeUrlUrlNil :: WriteUrl UrlNil params where
  writeUrl _ params = ""

instance writeUrlConsKey ::
  ( IsSymbol key
  , Row.Cons key valType restOfParams params
  , Row.Lacks key restOfParams
  , EncodeParam valType
  , WriteUrl rest restOfParams
  ) => WriteUrl (UrlCons (Key key) rest) params where
  writeUrl _ params = "/" <> encodedParam <> restOfUrl
    where
      encodedParam = encodeParam (Record.get (SProxy :: SProxy key) params)
      restOfParams = Record.delete (SProxy :: _ key) params
      restOfUrl = writeUrl (UrlListProxy :: _ rest) restOfParams

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
  , Row.Cons multiKey (List String) () params
  ) => WriteUrl (UrlCons (Multi multiKey) UrlNil) params where
  writeUrl _ params = "/" <> multiStr
    where
      multiList = Record.get (SProxy :: _ multiKey) params
      multiStr = String.joinWith "/" (Array.fromFoldable multiList)
