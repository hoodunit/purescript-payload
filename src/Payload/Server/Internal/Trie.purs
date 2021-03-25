module Payload.Server.Internal.Trie where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), either, note)
import Data.Foldable (class Foldable, foldM)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Payload.Internal.UrlParsing (Segment(Lit, Key, Multi))

data Trie a = Trie
  { value :: Maybe a
  , children :: List (Tuple Segment (Trie a)) }

instance showTrie :: Show a => Show (Trie a) where
  show (Trie t) = show t

instance eqTrie :: Eq a => Eq (Trie a) where
  eq (Trie { value: v1, children: c1 }) (Trie { value: v2, children: c2 }) = v1 == v2 && c1 == c2

instance functorTrie :: Functor Trie where
  map f (Trie { value, children }) = Trie { value: f <$> value, children: mapChild <$> children }
    where
      -- mapChild :: forall a. Tuple Segment (Trie a) -> Tuple Segment (Trie a)
      mapChild (Tuple s trie) = Tuple s (f <$> trie)

insert :: forall a. a -> List Segment -> Trie a -> Either String (Trie a)
insert newVal Nil (Trie { value: Nothing, children }) = Right (Trie { value: Just newVal, children })
insert newVal Nil (Trie { value: Just _ }) = Left $ "Failed to insert: duplicate key"
insert newVal (seg : rest) trie@(Trie { value, children }) =
  case List.findIndex (segmentMatches seg) children of
    Just matchIndex -> do
      matchingChild <- note "No index of match index??" $ List.index children matchIndex
      updatedChild <- updateChild matchingChild
      updatedChildren <- note "Could not update child??" $ List.updateAt matchIndex updatedChild children
      Right (Trie { value, children: updatedChildren })
    -- No matching segment found -> check for collisions, then add to list if no collisions
    Nothing -> if existsSegmentCollision seg children
                  then Left "Collision while recursing down trie"
                  else (\newChild -> Trie { value, children: (Tuple seg newChild) : children }) <$> insert newVal rest empty
  where
    updateChild :: Tuple Segment (Trie a) -> Either String (Tuple Segment (Trie a))
    updateChild (Tuple s child) = Tuple s <$> insert newVal rest child

    existsSegmentCollision :: Segment -> List (Tuple Segment (Trie a)) -> Boolean
    existsSegmentCollision s1 = List.any (\(Tuple s2 _) -> segmentCollides s1 s2)

segmentCollides :: Segment -> Segment -> Boolean
segmentCollides (Lit s1) (Lit s2) = s1 == s2
segmentCollides (Key _) (Key _) = true
segmentCollides (Multi _) (Multi _) = true
segmentCollides _ _ = false

segmentMatches :: forall a. Segment -> Tuple Segment (Trie a) -> Boolean
segmentMatches s1 (Tuple s2 _) = s1 == s2

empty :: forall a. Trie a
empty = Trie { value: Nothing, children: Nil }

newLeaf :: forall a. a -> Trie a
newLeaf value = Trie { value: Just value, children: Nil }

lookup :: forall a. List String -> Trie a -> List a
lookup Nil (Trie { value: Nothing }) = Nil
lookup Nil (Trie { value: Just value }) = value : Nil
lookup (key : rest) (Trie { value, children }) =
  List.concatMap (matchingChildren key) children
  # List.sortBy (\(Tuple s1 _) (Tuple s2 _) -> s1 `compare` s2)
  # map Tuple.snd
  where
    matchingChildren :: String -> Tuple Segment (Trie a) -> List (Tuple Segment a)
    matchingChildren match (Tuple seg@(Lit lit) child) =
      if match == lit
        then (Tuple seg) <$> lookup rest child
        else Nil
    matchingChildren match (Tuple seg@(Key _) child) = Tuple seg <$> lookup rest child
    matchingChildren match (Tuple seg@(Multi _) (Trie { value: Just val })) = (Tuple seg val) : Nil
    matchingChildren match (Tuple seg@(Multi _) (Trie { value: Nothing })) = Nil


lookup_ :: forall f a. Foldable f => f String -> Trie a -> List a
lookup_ f trie = lookup (List.fromFoldable f) trie

fromFoldable :: forall f a l
  . Foldable f
  => Foldable l
  => f (Tuple (l Segment) a) -> Either String (Trie a)
fromFoldable f = foldM (\trie (Tuple list val) -> insert val (List.fromFoldable list) trie) empty f

fromFoldable_ :: forall f a l
  . Foldable f
  => Foldable l
  => f (Tuple (l Segment) a) -> Trie a
fromFoldable_ = fromFoldable >>> either (const empty) identity

toList :: forall a. Trie a -> List a
toList (Trie { value, children }) =
  (maybe Nil pure value) <> List.concatMap toList (Tuple.snd <$> children)

dumpEntries :: forall a. Show a => Trie a -> String
dumpEntries =
  toList
  >>> map show
  >>> Array.fromFoldable
  >>> String.joinWith "\n"
