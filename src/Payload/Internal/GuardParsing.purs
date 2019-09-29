module Payload.Internal.GuardParsing where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Debug.Trace as Debug
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Above, Beside, Text, kind Doc)

infixr 2 type Beside as <>
infixr 1 type Above as |>

data Guards (g :: GuardList) = Guards

foreign import kind GuardList
foreign import data GNil :: GuardList
foreign import data GCons :: Symbol -> GuardList -> GuardList

infixr 1 type GCons as :
type Nil = GNil

data GuardTypes types = GuardTypes

class Append (left :: GuardList) (right :: GuardList) (both :: GuardList) | left right -> both

instance appendGuardLeftNil :: Append GNil right right
instance appendGuards ::
  ( Append rest (GCons s right) both
  ) => Append (GCons s rest) right both

class ParseGuardList (string :: Symbol) (parts :: GuardList) | string -> parts
instance aNilParse :: ParseGuardList "" GNil
else instance bConsParse ::
  ( Symbol.Cons h t string
  , Match string h t "" "start" fl
  ) => ParseGuardList string fl

class Match
  (guards :: Symbol)
  (head :: Symbol)
  (tail :: Symbol)
  (acc :: Symbol)
  (mode :: Symbol)
  (out :: GuardList)
  | head tail acc mode -> out

instance startBracket ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "guard" rest
  ) => Match u "[" xs "" "start" rest
else instance failNoBracketAtStart ::
  ( ParseError u xs (Text "Missing [ - guard list must start with [") doc
  ) => Match u x xs acc "start" rest
else instance endEmptyGuardsAtBracket :: Match u "]" "" "" "guard" GNil
else instance endGuardsAtBracket :: Match u "]" "" acc "guard" (GCons acc GNil)
else instance failEndWithoutBracket ::
  ( ParseError u "" (Text "Guard list must end with ]") doc
  ) => Match u x "" acc "guard" rest
else instance failAnythingAfterBracket ::
  ( ParseError u xs (Text "Guard list ended  at ] but there was more text after") doc
  ) => Match u "]" xs acc "guard" rest
else instance failEndEmptyGuardAtComma ::
  ( ParseError u xs (Text "Guard names cannot be empty") doc
  ) => Match u "," xs "" "guard" (GCons acc rest)
else instance endGuardAtComma ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "space" rest
  ) => Match u "," xs acc "guard" (GCons acc rest)
else instance skipSpace ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "guard" rest
  ) => Match u " " xs acc "space" rest
else instance failNoSpace ::
  ( ParseError u xs (Text "Guards must be separated by a comma and space (missing space after comma)") doc
  ) => Match u x xs acc "space" rest
else instance failSpaceWithoutComma ::
  ( ParseError u xs (Text "Guards must be separated by a comma and space (saw space without comma)") doc
  ) => Match u " " xs acc "guard" rest
else instance failOpenBracketInGuardName ::
  ( ParseError u xs (Text "Guard names cannot contain [") doc
  ) => Match u "[" xs acc "guard" rest
else instance contGuard ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc x newAcc
  , Match u y ys newAcc "guard" rest
  ) => Match u x xs acc "guard" rest
else instance failMatch ::
  ( ParseError u xs (Text "Failed parsing guards " <>
                     (Text "(acc: '" <> Text acc <> Text "', mode: " <> Text mode <> Text ")" )) doc
  ) => Match u x xs acc mode rest

class ParseError
  (path :: Symbol)
  (remaining :: Symbol)
  (msg :: Doc)
  (doc :: Doc)
  | path remaining msg -> doc

instance parseError ::
  ( Fail (Text "Invalid guard list: " <> msg
          |> Text ""
          |> Text "Guards: '" <> Text path <> Text "'"
          |> Text "--------" <> Text arrow <> Text "^"
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

toList :: forall guardsStr guards
  .  ParseGuardList guardsStr guards
  => ToGuardList guards
  => SProxy guardsStr -> List String
toList _ = List.reverse $ toGuardList (Guards :: _ guards) Nil

class ToGuardList (guardList :: GuardList) where
  toGuardList :: Guards guardList -> List String -> List String

instance toGuardListNil :: ToGuardList GNil where
  toGuardList _ acc = acc

instance toGuardListCons ::
  ( IsSymbol name
  , ToGuardList rest
  ) => ToGuardList (GCons name rest) where
  toGuardList _ names = toGuardList (Guards :: _ rest) (name : names)
    where
      name = reflectSymbol (SProxy :: SProxy name)
