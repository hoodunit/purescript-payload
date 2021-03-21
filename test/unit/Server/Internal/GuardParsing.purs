module Payload.Test.Unit.Server.Internal.GuardParsing where

import Prelude

import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Payload.Server.Internal.GuardParsing (class ParseGuardList, class ToGuardList)
import Payload.Server.Internal.GuardParsing as GuardParsing
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))

decode :: forall guardsStr guards
  . IsSymbol guardsStr
  => ParseGuardList guardsStr guards
  => ToGuardList guards
  => Proxy guardsStr -> List String -> TestSuite
decode guardsStr decoded = test ("'" <> (reflectSymbol guardsStr) <> "'") do
  Assert.equal decoded (GuardParsing.toList guardsStr)

tests :: TestSuite
tests = suite "Guard list parsing" do
  decode (Proxy :: _ "[]") Nil
  decode (Proxy :: _ "[a]") ("a" : Nil)
  decode (Proxy :: _ "[a, b]") ("a" : "b" : Nil)

  -- Should fail on compilation
  -- decode (Proxy :: _ "a, b]") Nil
  -- decode (Proxy :: _ "[a, b") Nil
  -- decode (Proxy :: _ "[a, b]c") Nil
  -- decode (Proxy :: _ "[a, b]c]") Nil
  -- decode (Proxy :: _ "[a b]") Nil
  -- decode (Proxy :: _ "[a,b]") Nil
  -- decode (Proxy :: _ "[a, , ]") Nil
  -- decode (Proxy :: _ "[a[]") Nil
