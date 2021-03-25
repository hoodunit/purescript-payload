-- | Helpers for dealing with request headers
module Payload.Headers
  ( Headers
  , empty
  , fromFoldable
  , lookup
  , set
  , setIfNotDefined
  , toUnfoldable
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Payload.Internal.Utils as Utils

newtype Headers = Headers (Map String String)

instance showHeaders :: Show Headers where
  show (Headers h) = show h

instance eqHeaders :: Eq Headers where
  eq (Headers a) (Headers b) = eq a b

-- | This prefers values on the right in the case
-- | of duplicate keys, i.e. old values are overwritten.
instance semigroupHeaders :: Semigroup Headers where
  append (Headers a) (Headers b) = Headers (Map.union b a)

empty :: Headers
empty = Headers Map.empty

set :: String -> String -> Headers -> Headers
set name value (Headers headers) =
  Headers (Map.insert (Utils.toLowerCase name) value headers)

setIfNotDefined :: String -> String -> Headers -> Headers
setIfNotDefined name value headers | member name headers = headers
setIfNotDefined name value headers = set name value headers

fromFoldable :: forall f. Foldable f => f (Tuple String String) -> Headers
fromFoldable f = foldl (\m (Tuple k v) -> set k v m) empty f

toUnfoldable :: forall f. Unfoldable f => Headers -> f (Tuple String String)
toUnfoldable (Headers h) = Map.toUnfoldable h

lookup :: String -> Headers -> Maybe String
lookup key (Headers h) = Map.lookup (Utils.toLowerCase key) h

member :: String -> Headers -> Boolean
member key (Headers h) = Map.member (Utils.toLowerCase key) h
