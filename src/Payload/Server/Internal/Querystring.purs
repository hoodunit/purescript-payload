module Payload.Server.Internal.Querystring where

import Prelude

import Data.Either (Either)
import Data.Symbol (SProxy)
import Foreign.Object (Object)
import Type.Proxy (Proxy)

foreign import querystringParse :: String -> Object (Array String)

class DecodeQuery (queryUrlSpec :: Symbol) query | queryUrlSpec -> query where
  decodeQuery :: SProxy queryUrlSpec -> Proxy (Record query) -> String -> Either String (Record query)

type ParsedQuery = Object (Array String)
