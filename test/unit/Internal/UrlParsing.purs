module Payload.Test.Unit.Internal.UrlParsing where

import Prelude

import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Payload.Internal.UrlParsing (class ParseUrl, class ToSegments, Segment(..), asSegments)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))

decode :: forall urlStr urlParts
  . IsSymbol urlStr
  => ParseUrl urlStr urlParts
  => ToSegments urlParts
  => Proxy urlStr -> List Segment -> TestSuite
decode path decoded = test ("'" <> (reflectSymbol path) <> "'") do
  Assert.equal decoded (asSegments path)

tests :: TestSuite
tests = suite "URL spec type-level parsing" do
  suite "literals" do
    decode (Proxy :: _ "/") Nil
    decode (Proxy :: _ "/users")(Lit "users" : Nil)
    decode (Proxy :: _ "/users/") (Lit "users" : Nil)
    decode (Proxy :: _ "/users/profile") (Lit "users" : Lit "profile" : Nil)
    decode (Proxy :: _ "/users/profile/") (Lit "users" : Lit "profile" : Nil)
    decode (Proxy :: _ "/users/profile/all")
           (Lit "users" : Lit "profile" : Lit "all" : Nil)
    decode (Proxy :: _ "/users//profile") (Lit "users" : Lit "" : Lit "profile" : Nil)

    -- Should fail at compile time:
    -- decode (Proxy :: _ "") Nil
    -- decode (Proxy :: _ "users") (Lit "users" : Nil)

  suite "keys" do
    decode (Proxy :: _ "/<userId>") (Key "userId" : Nil)
    decode (Proxy :: _ "/<userId>/") (Key "userId" : Nil)
    decode (Proxy :: _ "/users/<userId>") (Lit "users" : Key "userId" : Nil)
    decode (Proxy :: _ "/users/<userId>/posts")
           (Lit "users" : Key "userId" : Lit "posts" : Nil)
    decode (Proxy :: _ "/users/<userId>/posts/<postId>")
           (Lit "users" : Key "userId" : Lit "posts" : Key "postId" : Nil)

    -- Should fail at compile time:
    -- decode (Proxy :: _ "<userId>") (Key "userId" : Nil)
    -- decode (Proxy :: _ "<>") Nil
    -- decode (Proxy :: _ "/users/<>") Nil
    -- decode (Proxy :: _ "/users/<>/posts") Nil
    -- decode (Proxy :: _ "/users/<userId") Nil
    -- decode (Proxy :: _ "/users/<use<rId>") Nil
    -- decode (Proxy :: _ "/users/<user>Id>") Nil
    -- decode (Proxy :: _ "/users/<use<r>Id>") Nil
    -- decode (Proxy :: _ "/users/<userId>foo") Nil

  suite "multi" do
    decode (Proxy :: _ "/<..rest>") (Multi "rest" : Nil)
    decode (Proxy :: _ "/users/<id>/<..rest>") (Lit "users" : Key "id" : Multi "rest" : Nil)

    -- Should fail at compile time:
    -- decode (Proxy :: _ "/<..>") Nil
    -- decode (Proxy :: _ "/<..rest>/<user>") Nil
    -- decode (Proxy :: _ "/<..all") Nil
    -- decode (Proxy :: _ "/<..re<st>") Nil
    -- decode (Proxy :: _ "/<..re<a>st>") Nil
    -- decode (Proxy :: _ "/<..rea>st>") Nil

  suite "query" do
    decode (Proxy :: _ "/<..rest>?foo") (Multi "rest" : Nil)
