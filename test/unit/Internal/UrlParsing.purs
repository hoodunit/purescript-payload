module Payload.Test.Unit.Internal.UrlParsing where

import Prelude

import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Payload.Internal.UrlParsing (class ParseUrl, class ToSegments, Segment(..), asSegments)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

decode :: forall urlStr urlParts
  . IsSymbol urlStr
  => ParseUrl urlStr urlParts
  => ToSegments urlParts
  => SProxy urlStr -> List Segment -> TestSuite
decode path decoded = test ("'" <> (reflectSymbol path) <> "'") do
  Assert.equal decoded (asSegments path)

tests :: TestSuite
tests = suite "URL spec type-level parsing" do
  suite "literals" do
    decode (SProxy :: _ "/") Nil
    decode (SProxy :: _ "/users")(Lit "users" : Nil)
    decode (SProxy :: _ "/users/") (Lit "users" : Nil)
    decode (SProxy :: _ "/users/profile") (Lit "users" : Lit "profile" : Nil)
    decode (SProxy :: _ "/users/profile/") (Lit "users" : Lit "profile" : Nil)
    decode (SProxy :: _ "/users/profile/all")
           (Lit "users" : Lit "profile" : Lit "all" : Nil)
    decode (SProxy :: _ "/users//profile") (Lit "users" : Lit "" : Lit "profile" : Nil)

    -- Should fail at compile time:
    -- decode (SProxy :: _ "") Nil
    -- decode (SProxy :: _ "users") (Lit "users" : Nil)

  suite "keys" do
    decode (SProxy :: _ "/<userId>") (Key "userId" : Nil)
    decode (SProxy :: _ "/<userId>/") (Key "userId" : Nil)
    decode (SProxy :: _ "/users/<userId>") (Lit "users" : Key "userId" : Nil)
    decode (SProxy :: _ "/users/<userId>/posts")
           (Lit "users" : Key "userId" : Lit "posts" : Nil)
    decode (SProxy :: _ "/users/<userId>/posts/<postId>")
           (Lit "users" : Key "userId" : Lit "posts" : Key "postId" : Nil)

    -- Should fail at compile time:
    -- decode (SProxy :: _ "<userId>") (Key "userId" : Nil)
    -- decode (SProxy :: _ "<>") Nil
    -- decode (SProxy :: _ "/users/<>") Nil
    -- decode (SProxy :: _ "/users/<>/posts") Nil
    -- decode (SProxy :: _ "/users/<userId") Nil
    -- decode (SProxy :: _ "/users/<use<rId>") Nil
    -- decode (SProxy :: _ "/users/<user>Id>") Nil
    -- decode (SProxy :: _ "/users/<use<r>Id>") Nil
    -- decode (SProxy :: _ "/users/<userId>foo") Nil

  suite "multi" do
    decode (SProxy :: _ "/<..rest>") (Multi "rest" : Nil)
    decode (SProxy :: _ "/users/<id>/<..rest>") (Lit "users" : Key "id" : Multi "rest" : Nil)

    -- Should fail at compile time:
    -- decode (SProxy :: _ "/<..>") Nil
    -- decode (SProxy :: _ "/<..rest>/<user>") Nil
    -- decode (SProxy :: _ "/<..all") Nil
    -- decode (SProxy :: _ "/<..re<st>") Nil
    -- decode (SProxy :: _ "/<..re<a>st>") Nil
    -- decode (SProxy :: _ "/<..rea>st>") Nil

  suite "query" do
    decode (SProxy :: _ "/<..rest>?foo") (Multi "rest" : Nil)
