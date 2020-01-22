module Payload.Server.Internal.GuardParsing where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Payload.TypeErrors (class PrintArrow, type (<>), type (|>))
import Payload.Spec (SCons, SNil, Guards(Guards), kind SList)
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Text, kind Doc)

foreign import data GuardParseFail :: SList

data GuardTypes types = GuardTypes

class Append (left :: SList) (right :: SList) (both :: SList) | left right -> both

instance appendGuardLeftNil :: Append SNil right right
instance appendGuards ::
  ( Append rest (SCons s right) both
  ) => Append (SCons s rest) right both

class ParseGuardList (string :: Symbol) (parts :: SList) | string -> parts
instance aNilParse :: ParseGuardList "" SNil
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
  (out :: SList)
  | head tail acc mode -> out

instance startBracket ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "guard" rest
  ) => Match u "[" xs "" "start" rest
else instance failNoBracketAtStart ::
  ( ParseError u xs (Text "Missing [ - guard list must start with [") doc
  ) => Match u x xs acc "start" GuardParseFail
else instance endEmptyGuardsAtBracket :: Match u "]" "" "" "guard" SNil
else instance endGuardsAtBracket :: Match u "]" "" acc "guard" (SCons acc SNil)
else instance failEndWithoutBracket ::
  ( ParseError u "" (Text "Guard list must end with ]") doc
  ) => Match u x "" acc "guard" GuardParseFail
else instance failAnythingAfterBracket ::
  ( ParseError u xs (Text "Guard list ended  at ] but there was more text after") doc
  ) => Match u "]" xs acc "guard" GuardParseFail
else instance failEndEmptyGuardAtComma ::
  ( ParseError u xs (Text "Guard names cannot be empty") doc
  ) => Match u "," xs "" "guard" (SCons acc rest)
else instance endGuardAtComma ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "space" rest
  ) => Match u "," xs acc "guard" (SCons acc rest)
else instance skipSpace ::
  ( Symbol.Cons y ys xs
  , Match u y ys "" "guard" rest
  ) => Match u " " xs acc "space" rest
else instance failNoSpace ::
  ( ParseError u xs (Text "Guards must be separated by a comma and space (missing space after comma)") doc
  ) => Match u x xs acc "space" GuardParseFail
else instance failSpaceWithoutComma ::
  ( ParseError u xs (Text "Guards must be separated by a comma and space (saw space without comma)") doc
  ) => Match u " " xs acc "guard" GuardParseFail
else instance failOpenBracketInGuardName ::
  ( ParseError u xs (Text "Guard names cannot contain [") doc
  ) => Match u "[" xs acc "guard" GuardParseFail
else instance contGuard ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc x newAcc
  , Match u y ys newAcc "guard" rest
  ) => Match u x xs acc "guard" rest
else instance failMatch ::
  ( ParseError u xs (Text "Failed parsing guards " <>
                     (Text "(acc: '" <> Text acc <> Text "', mode: " <> Text mode <> Text ")" )) doc
  ) => Match u x xs acc mode GuardParseFail

class ParseError
  (path :: Symbol)
  (remaining :: Symbol)
  (msg :: Doc)
  (doc :: Doc)
  | path remaining msg -> doc

instance parseError ::
  ( Fail (Text "Invalid spec guard list: " <> msg
          |> Text ""
          |> Text "Guards: '" <> Text path <> Text "'"
          |> Text "--------" <> Text arrow <> Text "^"
         )
  , Symbol.Append start rem path
  , PrintArrow path start "" arrow
  ) => ParseError path rem msg doc

toList :: forall guardsStr guards
  .  ParseGuardList guardsStr guards
  => ToGuardList guards
  => SProxy guardsStr -> List String
toList _ = List.reverse $ toGuardList (Guards :: _ guards) Nil

class ToGuardList (guardList :: SList) where
  toGuardList :: Guards guardList -> List String -> List String

instance toGuardListNil :: ToGuardList SNil where
  toGuardList _ acc = acc

instance toGuardListCons ::
  ( IsSymbol name
  , ToGuardList rest
  ) => ToGuardList (SCons name rest) where
  toGuardList _ names = toGuardList (Guards :: _ rest) (name : names)
    where
      name = reflectSymbol (SProxy :: SProxy name)
