module Payload.TypeErrors where

import Prim.Symbol as Symbol
import Prim.TypeError (Above, Beside)

infixr 2 type Beside as <>
infixr 1 type Above as |>

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
