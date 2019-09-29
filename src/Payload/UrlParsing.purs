module Payload.UrlParsing where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Above, Beside, Text, kind Doc)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)

infixr 2 type Beside as <>
infixr 1 type Above as |>

debugShowUrl :: forall urlStr urlParts
  .  ParseUrl urlStr urlParts
  => ShowUrl urlParts
  => SProxy urlStr -> String
debugShowUrl _ = showUrl (UrlListProxy :: _ urlParts) ""

class ShowUrl (urlParts :: UrlList) where
  showUrl :: UrlListProxy urlParts -> String -> String

instance showUrlUrlNil :: ShowUrl UrlNil where
  showUrl _ acc = acc

instance showUrlConsKey ::
  ( IsSymbol key
  , ShowUrl rest
  ) => ShowUrl (UrlCons (Key key) rest) where
  showUrl _ str = showUrl (UrlListProxy :: _ rest) (str <> "(key: " <> keyStr <> ")")
    where
      keyStr = reflectSymbol (SProxy :: SProxy key)

instance showUrlConsMulti ::
  ( IsSymbol multi
  , ShowUrl rest
  ) => ShowUrl (UrlCons (Multi multi) rest) where
  showUrl _ str = showUrl (UrlListProxy :: _ rest) (str <> "(multi: " <> multiStr <> ")")
    where
      multiStr = reflectSymbol (SProxy :: SProxy multi)

instance showUrlConsLit ::
  ( IsSymbol lit
  , ShowUrl rest
  ) => ShowUrl (UrlCons (Lit lit) rest) where
  showUrl _ str = showUrl (UrlListProxy :: _ rest) (str <> "(lit: " <> litStr <> ")")
    where
      litStr = reflectSymbol (SProxy :: SProxy lit)

--------------------------------------------------------------------------------

data Segment = Lit String | Key String | Multi String

instance eqSegment :: Eq Segment where
  eq (Lit a) (Lit b) = a == b
  eq (Key a) (Key b) = a == b
  eq (Multi a) (Multi b) = a == b
  eq _ _ = false

instance showSegment :: Show Segment where
  show (Lit a) = "(Lit '" <> a <> "')"
  show (Key a) = "(Key '" <> a <> "')"
  show (Multi a) = "(Multi '" <> a <> "')"
  -- show (Lit a) = "/" <> a
  -- show (Key a) = "/<" <> a <> ">"
  -- show (Multi a) = "/<" <> a <> "..>"

instance ordSegment :: Ord Segment where
  compare (Lit a) (Lit b) = a `compare` b
  compare (Key a) (Key b) = a `compare` b
  compare (Multi a) (Multi b) = a `compare` b
  compare a b = rank a `compare` rank b
    where
      rank (Lit _) = 1
      rank (Key _) = 2
      rank (Multi _) = 3

asSegments :: forall urlStr urlParts
  .  ParseUrl urlStr urlParts
  => ToSegments urlParts
  => SProxy urlStr -> List Segment
asSegments _ = List.reverse $ toSegments (UrlListProxy :: _ urlParts) Nil

class ToSegments (urlParts :: UrlList) where
  toSegments :: UrlListProxy urlParts -> List Segment -> List Segment

instance toSegmentsUrlNil :: ToSegments UrlNil where
  toSegments _ acc = acc

instance toSegmentsConsKey ::
  ( IsSymbol key
  , ToSegments rest
  ) => ToSegments (UrlCons (Key key) rest) where
  toSegments _ segs = toSegments (UrlListProxy :: _ rest) (Key keyStr : segs)
    where
      keyStr = reflectSymbol (SProxy :: SProxy key)

instance toSegmentsConsMulti ::
  ( IsSymbol multi
  , ToSegments rest
  ) => ToSegments (UrlCons (Multi multi) rest) where
  toSegments _ segs = toSegments (UrlListProxy :: _ rest) (Multi multiStr : segs)
    where
      multiStr = reflectSymbol (SProxy :: SProxy multi)

instance toSegmentsConsLit ::
  ( IsSymbol lit
  , ToSegments rest
  ) => ToSegments (UrlCons (Lit lit) rest) where
  toSegments _ segs = toSegments (UrlListProxy :: _ rest) (Lit litStr : segs)
    where
      litStr = reflectSymbol (SProxy :: SProxy lit)

--------------------------------------------------------------------------------

data UrlListProxy (f :: UrlList) = UrlListProxy

foreign import kind UrlList
foreign import data UrlNil :: UrlList
foreign import data UrlCons :: UrlPart -> UrlList -> UrlList

class ParseUrl (string :: Symbol) (parts :: UrlList) | string -> parts
instance aNilParse :: ParseUrl "" UrlNil
else instance bConsParse ::
  ( Symbol.Cons h t string
  , Match string h t "" "start" fl
  ) => ParseUrl string fl

class Match
  (url :: Symbol)
  (head :: Symbol)
  (tail :: Symbol)
  (acc :: Symbol)
  (mode :: Symbol)
  (out :: UrlList)
  | head tail acc mode -> out

-- Generic/ending matches --------------------------------------
instance endEmptyTrailingSlashLit :: Match u "/" "" "" mode UrlNil
else instance endTrailingSlashLit :: Match u "/" "" acc "lit" (UrlCons (Lit acc) UrlNil)
else instance startSlash ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "any" rest
  ) => Match u "/" xs acc "start" rest
else instance failNoSlashAtStart ::
  ( Symbol.Cons x xs fullUrl
  , ParseError u xs "Missing / - path segments must start with / and be separated by /" doc
  ) => Match u x xs acc "start" rest

-- Multi ----------------------------------------------------
else instance failEmptyMulti ::
  ( ParseError u xs "multi-segment matches must have a name" doc
  ) => Match u ">" xs "" "multi" rest
else instance endAtMulti ::
  Match u ">" "" acc "multi" (UrlCons (Multi acc) UrlNil)
else instance queryAfterMulti ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "end" rest
  ) => Match u ">" xs acc "multi" (UrlCons (Multi acc) UrlNil)
else instance failMissingMultiEnd ::
  ( ParseError u "" "multi tag was not closed" doc
  ) => Match u x "" acc "multi" rest
else instance failNestedOpenMulti ::
  ( ParseError u xs "tags cannot be nested in multi tags" doc
  ) => Match u "<" xs acc "multi" rest
else instance contMulti ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc x newAcc
  , Match u y ys newAcc "multi" rest
  ) => Match u x xs acc "multi" rest

-- Keys --------------------------------------------------------
else instance startKey ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "key" rest
  ) => Match u "<" xs "" "any" rest
else instance switchKeyToMulti ::
  ( Symbol.Cons "." ys xs
  , Symbol.Cons z zs ys
  , Match u z zs "" "multi" rest
  ) => Match u "." xs "" "key" rest
else instance failEmptyKey ::
  ( ParseError u xs "key matches must have name" doc
  ) => Match u ">" xs "" "key" rest
else instance endAtKey ::
  Match u ">" "" acc "key" (UrlCons (Key acc) UrlNil)
else instance endKey ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "start" rest
  ) => Match u ">" xs acc "key" (UrlCons (Key acc) rest)
else instance failMissingKeyEnd ::
  ( Symbol.Append acc x key
  , ParseError u "" "key tag was not closed" doc
  ) => Match u x "" acc "key" rest
else instance failNestedOpenKey ::
  ( ParseError u xs "key tags cannot be nested" doc
  ) => Match u "<" xs acc "key" rest
else instance contKey ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc x newAcc
  , Match u y ys newAcc "key" rest
  ) => Match u x xs acc "key" rest
else instance failEndKeyWithoutStart ::
  ( ParseError u xs "saw closing '>' for key without opening '<'" doc
  ) => Match u ">" xs acc mode rest

-- Query ----------------------------------------------------
else instance endLitAtQuery :: Match u "?" xs acc "lit" (UrlCons (Lit acc) UrlNil)
else instance endAtQuery :: Match u "?" xs "" any UrlNil

-- Literals ----------------------------------------------------
else instance emptyLit ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "any" rest
  ) => Match u "/" xs "" "any" (UrlCons (Lit "") rest)
else instance startLit ::
  ( Symbol.Cons y ys xs
  , Match u y ys x "lit" rest
  ) => Match u x xs "" "any" rest
else instance splitLit ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "any" rest
  ) => Match u "/" xs acc "lit" (UrlCons (Lit acc) rest)
else instance endAtLit ::
  ( Symbol.Append acc x newAcc
  ) => Match u x "" acc "lit" (UrlCons (Lit newAcc) UrlNil)
else instance contLit ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc x newAcc
  , Match u y ys newAcc "lit" rest
  ) => Match u x xs acc "lit" rest

-- Generic failure ---------------------------------------------
else instance failMatch ::
  ( Fail
    ( Text "Failed parsing URL at: '" <> Text h <> Text "':'" <> Text t <>
      Text "' (acc: '" <> Text acc <> Text "', mode: " <> Text mode <> Text ")" )
  ) => Match u h t acc mode rest

foreign import kind UrlPart
foreign import data Key :: Symbol -> UrlPart
foreign import data Lit :: Symbol -> UrlPart
foreign import data Multi :: Symbol -> UrlPart

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
