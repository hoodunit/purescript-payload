module Payload.Test.Unit.Internal.QueryParsing where

import Prelude

import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Payload.Internal.QueryParsing (class ParseQuery, class ToSegments, Segment(..), asSegments)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

decode :: forall urlStr urlParts
  . IsSymbol urlStr
  => ParseQuery urlStr urlParts
  => ToSegments urlParts
  => SProxy urlStr -> List Segment -> TestSuite
decode path decoded = test ("'" <> (reflectSymbol path) <> "'") do
  Assert.equal decoded (asSegments path)

tests :: TestSuite
tests = suite "Query type-level parsing" do
  suite "keys" do
    decode (SProxy :: _ "/foo?foo=<myFoo>") (Key "foo" "myFoo" : Nil)
    decode (SProxy :: _ "/foo?foo=<myFoo>&bar=<myBar>")
      (Key "foo" "myFoo" : Key "bar" "myBar" : Nil)
    decode (SProxy :: _ "/foo?foo=<myFoo>&bar=<myBar>&qux=<myQux>")
      (Key "foo" "myFoo" : Key "bar" "myBar" : Key "qux" "myQux" : Nil)

    -- Should fail at compile time:
    -- decode (SProxy :: _ "/foo?<userId>") Nil
    -- decode (SProxy :: _ "/foo?session") Nil
    -- decode (SProxy :: _ "/foo?session=<>") Nil
    -- decode (SProxy :: _ "/foo?session=<session") Nil
    -- decode (SProxy :: _ "/foo?session=session>") Nil
    -- decode (SProxy :: _ "/foo?session=<ses<sion>") Nil
    -- decode (SProxy :: _ "/foo?session=<ses>sion>") Nil
    -- decode (SProxy :: _ "/foo?session=<session><..rest>") Nil

  suite "multi" do
    decode (SProxy :: _ "/foo?<..rest>") (Multi "rest" : Nil)
    decode (SProxy :: _ "/foo?a=<a>&<..rest>") (Key "a" "a" : Multi "rest" : Nil)

    -- Should fail at compile time:
    -- decode (SProxy :: _ "/?<..>") Nil
    -- decode (SProxy :: _ "/?<..rest>&foo") Nil
    -- decode (SProxy :: _ "/?<..all") Nil
    -- decode (SProxy :: _ "/?<..re<st>") Nil
    -- decode (SProxy :: _ "/?<..re<a>st>") Nil
    -- decode (SProxy :: _ "/?<..rea>st>") Nil
