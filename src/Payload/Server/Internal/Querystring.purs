module Payload.Server.Internal.Querystring where

import Data.Either (Either)
import Foreign.Object (Object)
import Type.Proxy (Proxy)

foreign import querystringParse :: String -> Object (Array String)

class DecodeQuery (queryUrlSpec :: Symbol) query | queryUrlSpec -> query where
  decodeQuery :: Proxy queryUrlSpec -> Proxy (Record query) -> String -> Either String (Record query)

type ParsedQuery = Object (Array String)
