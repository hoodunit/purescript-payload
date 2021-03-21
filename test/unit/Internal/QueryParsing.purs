module Payload.Test.Unit.Internal.QueryParsing where

import Prelude

import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Payload.Internal.QueryParsing (class ParseQuery, class ToSegments, Segment(..), asSegments)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))

decode :: forall urlStr urlParts
  . IsSymbol urlStr
  => ParseQuery urlStr urlParts
  => ToSegments urlParts
  => Proxy urlStr -> List Segment -> TestSuite
decode path decoded = test ("'" <> (reflectSymbol path) <> "'") do
  Assert.equal decoded (asSegments path)

tests :: TestSuite
tests = suite "Query type-level parsing" do
  suite "keys" do
    decode (Proxy :: _ "/foo?foo=<myFoo>") (Key "foo" "myFoo" : Nil)
    decode (Proxy :: _ "/foo?foo=<myFoo>&bar=<myBar>")
      (Key "foo" "myFoo" : Key "bar" "myBar" : Nil)
    decode (Proxy :: _ "/foo?foo=<myFoo>&bar=<myBar>&qux=<myQux>")
      (Key "foo" "myFoo" : Key "bar" "myBar" : Key "qux" "myQux" : Nil)

    -- Should fail at compile time:
    -- decode (Proxy :: _ "/foo?<userId>") Nil
    -- decode (Proxy :: _ "/foo?session") Nil
    -- decode (Proxy :: _ "/foo?session=<>") Nil
    -- decode (Proxy :: _ "/foo?session=<session") Nil
    -- decode (Proxy :: _ "/foo?session=session>") Nil
    -- decode (Proxy :: _ "/foo?session=<ses<sion>") Nil
    -- decode (Proxy :: _ "/foo?session=<ses>sion>") Nil
    -- decode (Proxy :: _ "/foo?session=<session><..rest>") Nil

  suite "multi" do
    decode (Proxy :: _ "/foo?<..rest>") (Multi "rest" : Nil)
    decode (Proxy :: _ "/foo?a=<a>&<..rest>") (Key "a" "a" : Multi "rest" : Nil)

    -- Should fail at compile time:
    -- decode (Proxy :: _ "/?<..>") Nil
    -- decode (Proxy :: _ "/?<..rest>&foo") Nil
    -- decode (Proxy :: _ "/?<..all") Nil
    -- decode (Proxy :: _ "/?<..re<st>") Nil
    -- decode (Proxy :: _ "/?<..re<a>st>") Nil
    -- decode (Proxy :: _ "/?<..rea>st>") Nil
