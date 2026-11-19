module Payload.Server.Internal.UrlString where

import Prelude

import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.String as String
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

foreign import unsafeDecodeURIComponent :: String -> String

urlToSegments :: String -> List String
urlToSegments = pathToSegments >>> (map unsafeDecodeURIComponent)

pathToSegments :: String -> List String
pathToSegments = dropEmpty <<< List.fromFoldable <<< String.split (wrap "/")
  where
    dropEmpty ("" : xs) = dropEmpty xs
    dropEmpty xs = xs

splitRequestTarget :: String -> { path :: String, query :: String }
splitRequestTarget target =
  case String.indexOf (wrap "?") pathAndQuery of
    Just i -> { path: String.take i pathAndQuery, query: String.drop (i + 1) pathAndQuery }
    Nothing -> { path: pathAndQuery, query: "" }
  where
    withoutFragment = maybe target (\i -> String.take i target) (String.indexOf (wrap "#") target)
    pathAndQuery = replace schemeAndAuthority "" withoutFragment

schemeAndAuthority :: Regex
schemeAndAuthority =
  unsafeRegex "^[A-Za-z][A-Za-z0-9+.-]*://[^/?]*" noFlags
