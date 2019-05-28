module Payload.Cookies where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex.Flags as RegexFlags
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.HTTP as HTTP
import Unsafe.Coerce (unsafeCoerce)

foreign import parseWrapper :: String -> Object String

requestCookies :: HTTP.Request -> Map String String
requestCookies req = case requestCookieHeader req of
  Just cookieHeader -> parseCookieHeader cookieHeader
  Nothing -> mempty

parseCookieHeader :: String -> Map String String
parseCookieHeader header = ((parseWrapper header
                           # Object.toUnfoldable) :: Array (Tuple String String))
                           # Map.fromFoldable

requestCookieHeader :: HTTP.Request -> Maybe String
requestCookieHeader = HTTP.requestHeaders >>> Object.lookup "cookie"
