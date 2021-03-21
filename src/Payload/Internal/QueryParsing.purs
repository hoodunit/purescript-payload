module Payload.Internal.QueryParsing where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Payload.TypeErrors (class PrintArrow, type (<>), type (|>))
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Text, Doc)
import Type.Prelude (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

data Segment = Key String String | Multi String

instance eqSegment :: Eq Segment where
  eq (Key name1 key1) (Key name2 key2) = name1 == name2 && key1 == key2
  eq (Multi a) (Multi b) = a == b
  eq _ _ = false

instance showSegment :: Show Segment where
  show (Key name key) = "(Key " <> name <> "=<" <> key <> ">)"
  show (Multi a) = "(Multi '" <> a <> "')"

instance ordSegment :: Ord Segment where
  compare (Key name1 key1) (Key name2 key2) =
    case name1 `compare` name2 of
      EQ -> key1 `compare` key2
      a -> a
  compare (Multi a) (Multi b) = a `compare` b
  compare a b = rank a `compare` rank b
    where
      rank (Key _ _) = 1
      rank (Multi _) = 2

asSegments :: forall urlStr urlParts
  .  ParseQuery urlStr urlParts
  => ToSegments urlParts
  => Proxy urlStr -> List Segment
asSegments _ = List.reverse $ toSegments (QueryListProxy :: _ urlParts) Nil

class ToSegments (urlParts :: QueryList) where
  toSegments :: QueryListProxy urlParts -> List Segment -> List Segment

instance toSegmentsQueryNil :: ToSegments QueryNil where
  toSegments _ acc = acc

instance toSegmentsConsKey ::
  ( IsSymbol name
  , IsSymbol key
  , ToSegments rest
  ) => ToSegments (QueryCons (Key name key) rest) where
  toSegments _ segs = toSegments (QueryListProxy :: _ rest) (Key nameStr keyStr : segs)
    where
      nameStr = reflectSymbol (Proxy :: Proxy name)
      keyStr = reflectSymbol (Proxy :: Proxy key)

instance toSegmentsConsMulti ::
  ( IsSymbol multi
  , ToSegments rest
  ) => ToSegments (QueryCons (Multi multi) rest) where
  toSegments _ segs = toSegments (QueryListProxy :: _ rest) (Multi multiStr : segs)
    where
      multiStr = reflectSymbol (Proxy :: Proxy multi)

--------------------------------------------------------------------------------

data QueryListProxy (f :: QueryList) = QueryListProxy

data QueryList
foreign import data QueryParseFail :: QueryList
foreign import data QueryNil :: QueryList
foreign import data QueryCons :: QueryPart -> QueryList -> QueryList

class ParseQuery (string :: Symbol) (parts :: QueryList) | string -> parts
instance aNilParse :: ParseQuery "" QueryNil
else instance bConsParse ::
  ( Symbol.Cons h t string
  , Match string h t "" "" "start" fl
  ) => ParseQuery string fl

class Match
  (url :: Symbol)
  (head :: Symbol)
  (tail :: Symbol)
  (acc :: Symbol)
  (acc2 :: Symbol)
  (mode :: Symbol)
  (out :: QueryList)
  | head tail acc mode -> out

-- Generic/ending matches --------------------------------------
instance failTrailingAmpersand ::
  ( ParseError u "" "Trailing & - query params cannot end with trailing &" doc
  ) => Match u "&" "" acc acc2 any QueryParseFail
else instance failEmptyQueryString ::
  ( ParseError u "" "Empty query string - there were no query parameters following '?'" doc
  ) => Match u "?" "" acc acc2 "start" QueryParseFail
else instance startQuestionMark ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "" "any" rest
  ) => Match u "?" xs acc acc2 "start" rest
else instance endWithEmptyNoQuestionMark :: Match u x "" "" acc2 "start" QueryNil
else instance skipNonQuestionMarkAtStart ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "" "start" rest
  ) => Match u x xs "" "" "start" rest
else instance failNoSlashAtStart ::
  ( Symbol.Cons x xs fullUrl
  , ParseError u xs "Missing ? - query segments must start with ? and be separated by &" doc
  ) => Match u x xs acc acc2 "start" QueryParseFail
else instance skipInBetweenAmpersand ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "" "any" rest
  ) => Match u "&" xs acc acc2 "any" rest

-- Multi ----------------------------------------------------
else instance startMulti ::
  ( Symbol.Cons "." ys xs
  , Symbol.Cons "." zs ys
  , Symbol.Cons q qs zs
  , Match u q qs "" "" "multi" rest
  ) => Match u "<" xs acc acc2 "any" rest
else instance failEmptyMulti ::
  ( ParseError u xs "multi-segment query matches must have a name" doc
  ) => Match u ">" xs "" acc2 "multi" QueryParseFail
else instance endAtMulti ::
  Match u ">" "" acc acc2 "multi" (QueryCons (Multi acc) QueryNil)
else instance failContinueAfterMulti ::
  ( ParseError u xs "multi-segment query match must be the final component of a path" doc
  ) => Match u ">" xs acc acc2 "multi" QueryParseFail
else instance failMissingMultiEnd ::
  ( ParseError u "" "multi-segment query match was not closed" doc
  ) => Match u x "" acc acc2 "multi" QueryParseFail
else instance failNestedOpenMulti ::
  ( ParseError u xs "tags cannot be nested in multi-segment query matches" doc
  ) => Match u "<" xs acc acc2 "multi" QueryParseFail
else instance contMulti ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc x newAcc
  , Match u y ys newAcc "" "multi" rest
  ) => Match u x xs acc "" "multi" rest

-- Keys --------------------------------------------------------
else instance keyEquals ::
  ( Symbol.Cons y ys xs
  , Match u y ys acc "" "key" rest
  ) => Match u "<" xs acc "" "keyEquals" rest
else instance failedKeyEquals ::
  ( ParseError u xs "query param key name must start with opening '<', e.g. limit=<limit>" doc
  ) => Match u x xs acc "" "keyEquals" QueryParseFail
else instance switchKeyToMulti ::
  ( Symbol.Cons "." ys xs
  , Symbol.Cons z zs ys
  , Match u z zs "" acc2 "multi" rest
  ) => Match u "." xs "" acc2 "key" rest
else instance failEmptyKey ::
  ( ParseError u xs "query param matches must have a name" doc
  ) => Match u ">" xs acc "" "key" QueryParseFail
else instance endAtKey ::
  Match u ">" "" name key "key" (QueryCons (Key name key) QueryNil)
else instance endKey ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "" "ampersand" rest
  ) => Match u ">" xs name key "key" (QueryCons (Key name key) rest)
else instance failMissingKeyEnd ::
  ( Symbol.Append acc x key
  , ParseError u "" "key tag was not closed" doc
  ) => Match u x "" acc acc2 "key" QueryParseFail
else instance failNestedOpenKey ::
  ( ParseError u xs "key tags cannot be nested" doc
  ) => Match u "<" xs acc acc2 "key" QueryParseFail
else instance contKey ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc2 x newAcc2
  , Match u y ys acc newAcc2 "key" rest
  ) => Match u x xs acc acc2 "key" rest

else instance ampersand ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "" "any" rest
  ) => Match u "&" xs name key "ampersand" rest
else instance ampersandFail ::
  ( ParseError u xs "expected '&' between query parts" doc
  ) => Match u x xs name key "ampersand" QueryParseFail

else instance switchToKeyEquals ::
  ( Symbol.Cons y ys xs
  , Match u y ys acc "" "keyEquals" rest
  ) => Match u "=" xs acc "" "lit" rest
else instance failEndKeyWithoutStart ::
  ( ParseError u xs "saw closing '>' for key without opening '<'" doc
  ) => Match u ">" xs acc acc2 mode QueryParseFail
else instance failOpenKeyWithoutEquals ::
  ( ParseError u xs "saw key name without query param name - query params should be of form name=<keyName>" doc
  ) => Match u "<" xs acc acc2 any QueryParseFail

-- Literals/query param names ----------------------------------
else instance startLit ::
  ( Symbol.Cons y ys xs
  , Match u y ys x "" "lit" rest
  ) => Match u x xs "" "" "any" rest
else instance failSplitLit ::
  ( ParseError u xs "saw & before query param name - query params should be of form name=<keyName>" doc
  ) => Match u "&" xs acc "" "lit" QueryParseFail
else instance failEndAtLit ::
  ( ParseError u "" "query string ended before query param name - query params should be of form name=<keyName>" doc
  ) => Match u x "" acc "" "lit" QueryParseFail
else instance contLit ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc x newAcc
  , Match u y ys newAcc "" "lit" rest
  ) => Match u x xs acc "" "lit" rest

-- Generic failure ---------------------------------------------
else instance failMatch ::
  ( Fail
    ( Text "Failed parsing URL at: '" <> Text h <> Text "':'" <> Text t <>
      Text "' (acc: '" <> Text acc <> Text "', acc2: '" <> Text acc2 <> Text "', mode: " <> Text mode <> Text ")" )
  ) => Match u h t acc acc2 mode rest

data QueryPart
foreign import data Key :: Symbol -> Symbol -> QueryPart
foreign import data Multi :: Symbol -> QueryPart

class ParseError
  (path :: Symbol)
  (remaining :: Symbol)
  (msg :: Symbol)
  (doc :: Doc)
  | path remaining msg -> doc

instance parseError ::
  ( Fail (Text "Invalid query spec: " <> Text msg
          |> Text ""
          |> Text "Path: '" <> Text path <> Text "'"
          |> Text "------" <> Text arrow <> Text "^"
         )
  , Symbol.Append start rem path
  , PrintArrow path start "" arrow
  ) => ParseError path rem msg doc
