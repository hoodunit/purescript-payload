module Payload.Server.Cookies
       ( cookieHeader
       , parseCookieHeader
       , requestCookies
       , setCookieHeader
       ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.HTTP as HTTP

foreign import parseWrapper :: String -> Object String
foreign import serializeImpl :: String -> String -> String

requestCookies :: HTTP.Request -> Map String String
requestCookies req = case requestCookieHeader req of
  Just header -> parseCookieHeader header
  Nothing -> Map.empty

parseCookieHeader :: String -> Map String String
parseCookieHeader header = ((parseWrapper header
                           # Object.toUnfoldable) :: Array (Tuple String String))
                           # Map.fromFoldable

requestCookieHeader :: HTTP.Request -> Maybe String
requestCookieHeader = HTTP.requestHeaders >>> Object.lookup "cookie"

setCookieHeader :: String -> String -> Tuple String String
setCookieHeader name val = Tuple "Set-Cookie" (serializeImpl name val)

cookieHeader :: Map String String -> Tuple String String
cookieHeader cookies = Tuple "Cookie" cookiesStr
  where
    cookiesStr = String.joinWith "; " (serialize <$> cookiesArray)
    serialize (Tuple key val) = serializeImpl key val
    cookiesArray :: Array (Tuple String String)
    cookiesArray = Map.toUnfoldable cookies
