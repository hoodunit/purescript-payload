module Payload.Server.Internal.UrlString where

import Prelude

import Data.List (List, (:))
import Data.List as List
import Data.Newtype (wrap)
import Data.String as String

foreign import unsafeDecodeURIComponent :: String -> String

urlToSegments :: String -> List String
urlToSegments = pathToSegments >>> (map unsafeDecodeURIComponent)

pathToSegments :: String -> List String
pathToSegments = dropEmpty <<< List.fromFoldable <<< String.split (wrap "/")
  where
    dropEmpty ("" : xs) = dropEmpty xs
    dropEmpty xs = xs
