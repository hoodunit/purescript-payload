module Payload.Test.Unit.Server.Internal.GuardParsing where

import Prelude

import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Payload.Server.Internal.GuardParsing (class ParseGuardList, class ToGuardList)
import Payload.Server.Internal.GuardParsing as GuardParsing
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

decode :: forall guardsStr guards
  . IsSymbol guardsStr
  => ParseGuardList guardsStr guards
  => ToGuardList guards
  => SProxy guardsStr -> List String -> TestSuite
decode guardsStr decoded = test ("'" <> (reflectSymbol guardsStr) <> "'") do
  Assert.equal decoded (GuardParsing.toList guardsStr)

tests :: TestSuite
tests = suite "Guard list parsing" do
  decode (SProxy :: _ "[]") Nil
  decode (SProxy :: _ "[a]") ("a" : Nil)
  decode (SProxy :: _ "[a, b]") ("a" : "b" : Nil)

  -- Should fail on compilation
  -- decode (SProxy :: _ "a, b]") Nil
  -- decode (SProxy :: _ "[a, b") Nil
  -- decode (SProxy :: _ "[a, b]c") Nil
  -- decode (SProxy :: _ "[a, b]c]") Nil
  -- decode (SProxy :: _ "[a b]") Nil
  -- decode (SProxy :: _ "[a,b]") Nil
  -- decode (SProxy :: _ "[a, , ]") Nil
  -- decode (SProxy :: _ "[a[]") Nil
