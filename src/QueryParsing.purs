module Payload.QueryParsing where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Above, Beside, Text, kind Doc)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)

infixr 2 type Beside as <>
infixr 1 type Above as |>

--------------------------------------------------------------------------------

data Segment = Lit String | Key String String | Multi String

instance eqSegment :: Eq Segment where
  eq (Lit a) (Lit b) = a == b
  eq (Key name1 key1) (Key name2 key2) = name1 == name2 && key1 == key2
  eq (Multi a) (Multi b) = a == b
  eq _ _ = false

instance showSegment :: Show Segment where
  show (Lit a) = "(Lit '" <> a <> "')"
  show (Key name key) = "(Key " <> name <> "=<" <> key <> ">)"
  show (Multi a) = "(Multi '" <> a <> "')"
  -- show (Lit a) = "/" <> a
  -- show (Key a) = "/<" <> a <> ">"
  -- show (Multi a) = "/<" <> a <> "..>"

instance ordSegment :: Ord Segment where
  compare (Lit a) (Lit b) = a `compare` b
  compare (Key name1 key1) (Key name2 key2) =
    case name1 `compare` name2 of
      EQ -> key1 `compare` key2
      a -> a
  compare (Multi a) (Multi b) = a `compare` b
  compare a b = rank a `compare` rank b
    where
      rank (Lit _) = 1
      rank (Key _ _) = 2
      rank (Multi _) = 3

asSegments :: forall urlStr urlParts
  .  ParseQuery urlStr urlParts
  => ToSegments urlParts
  => SProxy urlStr -> List Segment
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
      nameStr = reflectSymbol (SProxy :: SProxy name)
      keyStr = reflectSymbol (SProxy :: SProxy key)

instance toSegmentsConsMulti ::
  ( IsSymbol multi
  , ToSegments rest
  ) => ToSegments (QueryCons (Multi multi) rest) where
  toSegments _ segs = toSegments (QueryListProxy :: _ rest) (Multi multiStr : segs)
    where
      multiStr = reflectSymbol (SProxy :: SProxy multi)

instance toSegmentsConsLit ::
  ( IsSymbol lit
  , ToSegments rest
  ) => ToSegments (QueryCons (Lit lit) rest) where
  toSegments _ segs = toSegments (QueryListProxy :: _ rest) (Lit litStr : segs)
    where
      litStr = reflectSymbol (SProxy :: SProxy lit)

--------------------------------------------------------------------------------

data QueryListProxy (f :: QueryList) = QueryListProxy

foreign import kind QueryList
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
  ) => Match u "&" "" acc acc2 any QueryNil
else instance failEmptyQueryString ::
  ( ParseError u "" "Empty query string - there were no query parameters following '?'" doc
  ) => Match u "?" "" acc acc2 "start" rest
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
  ) => Match u x xs acc acc2 "start" rest
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
  ) => Match u ">" xs "" acc2 "multi" rest
else instance endAtMulti ::
  Match u ">" "" acc acc2 "multi" (QueryCons (Multi acc) QueryNil)
else instance failContinueAfterMulti ::
  ( ParseError u xs "multi-segment query match must be the final component of a path" doc
  ) => Match u ">" xs acc acc2 "multi" QueryNil
else instance failMissingMultiEnd ::
  ( ParseError u "" "multi-segment query match was not closed" doc
  ) => Match u x "" acc acc2 "multi" rest
else instance failNestedOpenMulti ::
  ( ParseError u xs "tags cannot be nested in multi-segment query matches" doc
  ) => Match u "<" xs acc acc2 "multi" rest
else instance contMulti ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc x newAcc
  , Match u y ys newAcc "" "multi" rest
  ) => Match u x xs acc "" "multi" rest

-- Keys --------------------------------------------------------
else instance keyStart ::
  ( Symbol.Cons y ys xs
  , Match u y ys acc "" "key" rest
  ) => Match u "<" xs acc "" "keyStart" rest
else instance failedKeyStart ::
  ( ParseError u xs "query param key name must start with opening '<', e.g. limit=<limit>" doc
  ) => Match u x xs acc "" "keyStart" rest
else instance switchKeyToMulti ::
  ( Symbol.Cons "." ys xs
  , Symbol.Cons z zs ys
  , Match u z zs "" acc2 "multi" rest
  ) => Match u "." xs "" acc2 "key" rest
else instance failEmptyKey ::
  ( ParseError u xs "query param matches must have a name" doc
  ) => Match u ">" xs acc "" "key" rest
else instance endAtKey ::
  Match u ">" "" name key "key" (QueryCons (Key name key) QueryNil)
else instance endKey ::
  ( Symbol.Cons "&" ys xs
  , Symbol.Cons z zs ys
  , Match u z zs "" "" "any" rest
  ) => Match u ">" xs name key "key" (QueryCons (Key name key) rest)
else instance failMissingKeyEnd ::
  ( Symbol.Append acc x key
  , ParseError u "" "key tag was not closed" doc
  ) => Match u x "" acc acc2 "key" rest
else instance failNestedOpenKey ::
  ( ParseError u xs "key tags cannot be nested" doc
  ) => Match u "<" xs acc acc2 "key" rest
else instance contKey ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc2 x newAcc2
  , Match u y ys acc newAcc2 "key" rest
  ) => Match u x xs acc acc2 "key" rest

else instance switchToKey ::
  ( Symbol.Cons y ys xs
  , Match u y ys acc "" "keyStart" rest
  ) => Match u "=" xs acc "" "lit" rest
else instance failEndKeyWithoutStart ::
  ( ParseError u xs "saw closing '>' for key without opening '<'" doc
  ) => Match u ">" xs acc acc2 mode rest
else instance failOpenKeyWithoutEquals ::
  ( ParseError u xs "saw key name without query param name - should be of form name=<keyName>" doc
  ) => Match u "<" xs acc acc2 any rest

-- Literals ----------------------------------------------------
else instance startLit ::
  ( Symbol.Cons y ys xs
  , Match u y ys x "" "lit" rest
  ) => Match u x xs "" "" "any" rest
-- else instance switchToKey ::
--   ( Symbol.Cons "<" ys xs
--   , Symbol.Cons z zs ys
--   , Match u z zs acc "" "key" rest
--   ) => Match u "=" xs acc "" "lit" rest
-- else instance failedSwitchToKey ::
--   ( ParseError u xs "invalid query parameter syntax - keys should be of form name=<keyName>" doc
--   ) => Match u "=" xs acc "" "lit" rest
else instance splitLit ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "" "any" rest
  ) => Match u "&" xs acc "" "lit" (QueryCons (Lit acc) rest)
else instance endAtLit ::
  ( Symbol.Append acc x newAcc
  ) => Match u x "" acc "" "lit" (QueryCons (Lit newAcc) QueryNil)
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

foreign import kind QueryPart
foreign import data Key :: Symbol -> Symbol -> QueryPart
foreign import data Lit :: Symbol -> QueryPart
foreign import data Multi :: Symbol -> QueryPart

class ParseError
  (path :: Symbol)
  (remaining :: Symbol)
  (msg :: Symbol)
  (doc :: Doc)
  | path remaining msg -> doc

instance parseError ::
  ( Fail (Text "Invalid route path: " <> Text msg
          |> Text ""
          |> Text "Path: '" <> Text path <> Text "'"
          |> Text "------" <> Text arrow <> Text "^"
         )
  , Symbol.Append start rem path
  , PrintArrow path start "" arrow
  ) => ParseError path rem msg doc

class PrintArrow
  (path :: Symbol)
  (start :: Symbol)
  (acc :: Symbol)
  (arrow :: Symbol)
  | path start acc -> arrow

instance printArrow :: PrintArrow path "" acc acc
else instance accumArrow ::
  ( Symbol.Cons head tail start
  , Symbol.Cons "-" acc newAcc
  , PrintArrow path tail newAcc arrow
  ) => PrintArrow path start acc arrow
