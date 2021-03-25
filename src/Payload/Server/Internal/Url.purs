module Payload.Server.Internal.Url where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Payload.Server.Params (class DecodeParam, class DecodeSegments, decodeParam, decodeSegments)
import Payload.Internal.UrlParsing (class ParseUrl, UrlListProxy(..), Key, Lit, Multi, UrlCons, UrlNil, UrlList)
import Prim.Row as Row
import Record as Record
import Type.Equality (class TypeEquals, to)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

class DecodeUrl (urlStr :: Symbol) (params :: Row Type) where
  decodeUrl :: Proxy urlStr -> Proxy (Record params) -> List String -> Either String (Record params)

instance decodeUrlSymbol ::
  ( ParseUrl urlStr urlParts
  , MatchUrl urlParts params ()
  ) => DecodeUrl urlStr params where
  decodeUrl _ paramsType path = match (UrlListProxy :: _ urlParts) paramsType {} path

class MatchUrl (urlParts :: UrlList) params from | urlParts -> from where
  match :: UrlListProxy urlParts -> Proxy (Record params) -> Record from -> List String -> Either String (Record params)

instance matchUrlUrlNil ::
  ( TypeEquals (Record from) (Record params)
  ) => MatchUrl UrlNil params from where
  match _ _ params Nil = Right (to params)
  match _ _ _ path = Left $ "Path mismatch: Ran out of params when path still had '" <> show path <> "'"

instance matchUrlMulti ::
  ( IsSymbol key
  , Row.Cons key valType rest params
  , Row.Lacks key rest
  , DecodeSegments valType
  ) => MatchUrl (UrlCons (Multi key) UrlNil) params rest where
  match _ paramsType params segments = case decodeSegments segments of
    Left errors -> Left $ show errors
    Right decoded -> Right $ Record.insert (Proxy :: Proxy key) decoded params

instance matchUrlConsKey ::
  ( IsSymbol key
  , MatchUrl rest params from'
  , Row.Cons key valType params' params
  , Row.Cons key valType from from'
  , Row.Lacks key from
  , DecodeParam valType
  ) => MatchUrl (UrlCons (Key key) rest) params from where
  match _ paramsType params Nil = Left "Decoding error at key"
  match _ paramsType params (segment : rest) = case decodeParam segment of
    Left errors -> Left $ show errors
    Right decoded -> let newParams = Record.insert (Proxy :: Proxy key) decoded params in
      match (UrlListProxy :: _ rest) paramsType newParams rest

instance matchUrlConsLit ::
  ( IsSymbol lit
  , MatchUrl rest params from
  ) => MatchUrl (UrlCons (Lit lit) rest) params from where
  match _ paramsType params Nil = Left "Decoding error at literal"
  match _ paramsType params (segment : rest) =
    match (UrlListProxy :: _ rest) paramsType params rest
