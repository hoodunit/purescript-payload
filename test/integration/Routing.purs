module Payload.Test.Integration.Routing where

import Prelude

import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Payload.Server (pathToSegments)
import Payload.Trie as Trie
import Payload.UrlParsing (class ParseUrl, class ToSegments, asSegments)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

match :: forall urlStr urlParts
  .  ParseUrl urlStr urlParts
  => ToSegments urlParts
  => IsSymbol urlStr
  => SProxy urlStr -> String -> TestSuite
match route reqPath = test ("✓ " <> reflectSymbol route <> " - " <> reqPath) $ do
  let lookedUp = testLookup route reqPath
  Assert.assert "Match failed, expected success" (lookedUp == ("handler" : Nil))

noMatch :: forall urlStr urlParts
  .  ParseUrl urlStr urlParts
  => ToSegments urlParts
  => IsSymbol urlStr
  => SProxy urlStr -> String -> TestSuite
noMatch route reqPath = test ("✘ " <> reflectSymbol route <> " - " <> reqPath ) $ do
  let lookedUp = testLookup route reqPath
  Assert.assert "Match succeeded, expected failure" (lookedUp == Nil)

testLookup :: forall urlStr urlParts
  .  ParseUrl urlStr urlParts
  => ToSegments urlParts
  => IsSymbol urlStr
  => SProxy urlStr -> String -> List String
testLookup route reqPath = Trie.lookup (pathToSegments reqPath) routingTrie
  where
    routeSegments = asSegments (SProxy :: SProxy urlStr)
    routingTrie = Trie.fromFoldable_ [ Tuple routeSegments "handler" ]

tests :: TestSuite
tests = do
  suite "Route matching: routeMatches" do
    match (SProxy :: SProxy "/hello") "/hello" 
    noMatch (SProxy :: SProxy "/hello") "/hello/" 

    match (SProxy :: SProxy "/<a>") "/hello" 
    match (SProxy :: SProxy "/<a>") "/hi" 
    match (SProxy :: SProxy "/<a>") "/bobbbbbbbbbby" 
    match (SProxy :: SProxy "/<a>") "/weklsdfki" 
    noMatch (SProxy :: SProxy "/<a>") "/hello/" 
    noMatch (SProxy :: SProxy "/<a>") "/hello/asdf" 

    match (SProxy :: SProxy "/<a>/<b>") "/hello/" 
    match (SProxy :: SProxy "/<a>/<b>") "/hello/one" 
    match (SProxy :: SProxy "/<a>/<b>") "/hello/i" 
    noMatch (SProxy :: SProxy "/<a>/<b>") "/hello" 
    noMatch (SProxy :: SProxy "/<a>/<b>") "/hello/there/one" 
    noMatch (SProxy :: SProxy "/<a>/<b>") "/hello/there/" 

    match (SProxy :: SProxy "/users/<id>/posts") "/users/1/posts" 
    match (SProxy :: SProxy "/users/<id>/posts") "/users/sde9823lsdle/posts" 
    noMatch (SProxy :: SProxy "/users/<id>/posts") "/users/1/post" 
    noMatch (SProxy :: SProxy "/users/<id>/posts") "/user/1/posts" 

    match (SProxy :: SProxy "/users/<..rest>") "/users/1/posts" 
    match (SProxy :: SProxy "/users/<..rest>") "/users/foo/bar/baz/qux" 
    match (SProxy :: SProxy "/users/<..rest>") "/users/" 
    noMatch (SProxy :: SProxy "/users/<..rest>") "/users" 

    match (SProxy :: SProxy "/users/<id>/<..rest>") "/users/12/a/b/c" 
    match (SProxy :: SProxy "/users/<id>/<..rest>") "/users/12/" 
    match (SProxy :: SProxy "/users/<id>/<..rest>") "/users/asdf/a" 
    noMatch (SProxy :: SProxy "/users/<id>/<..rest>") "/users/asdf" 

    -- URLs should be URL-decoded before reaching the matcher
    suite "URL decoding" do
      match (SProxy :: SProxy "/hello there") "/hello there" 
      noMatch (SProxy :: SProxy "/hello there") "/hello%20there" 
