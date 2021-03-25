module Payload.Test.Unit.Server.Cookies where

import Prelude

import Data.Map as Map
import Data.Tuple (Tuple(..))
import Payload.Server.Cookies as Cookies
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Cookies" do
  suite "parseCookieHeader" do
    test "parses" $ Assert.equal Map.empty (Cookies.parseCookieHeader "")
    test "basic" $ Assert.equal
      (Map.fromFoldable [Tuple "foo" "bar"])
      (Cookies.parseCookieHeader "foo=bar")
    test "basic #2" $ Assert.equal
      (Map.fromFoldable [Tuple "foo" "123"])
      (Cookies.parseCookieHeader "foo=123")
    test "multiple" $ Assert.equal
      (Map.fromFoldable [Tuple "FOO" "bar", Tuple "baz" "raz"])
      (Cookies.parseCookieHeader "FOO=bar;baz=raz")
    test "ignores spaces" $ Assert.equal
      (Map.fromFoldable [Tuple "foo" "123"])
      (Cookies.parseCookieHeader "foo    =    123")
    test "escaping" $ Assert.equal
      (Map.fromFoldable [Tuple "foo" "bar=123456789&name=Magic+Mouse"])
      (Cookies.parseCookieHeader "foo=\"bar=123456789&name=Magic+Mouse\"")
    test "escaping #2" $ Assert.equal
      (Map.fromFoldable [Tuple "email" " \",;/"])
      (Cookies.parseCookieHeader "email=%20%22%2c%3b%2f")
    test "ignores escaping error and returns original value" $ Assert.equal
      (Map.fromFoldable [Tuple "foo" "%1", Tuple "bar" "bar"])
      (Cookies.parseCookieHeader "foo=%1;bar=bar")
    test "ignores non values" $ Assert.equal
      (Map.fromFoldable [Tuple "foo" "%1", Tuple "bar" "bar"])
      (Cookies.parseCookieHeader "foo=%1;bar=bar;HttpOnly;Secure")
    test "ignores non values" $ Assert.equal
      (Map.fromFoldable [Tuple "foo" "%1", Tuple "bar" "bar"])
      (Cookies.parseCookieHeader "foo=%1;bar=bar;HttpOnly;Secure")
    test "dates" $ Assert.equal
      (Map.fromFoldable [Tuple "priority" "true", Tuple "Path" "/", Tuple "expires" "Wed, 29 Jan 2014 17:43:25 GMT"])
      (Cookies.parseCookieHeader "priority=true; expires=Wed, 29 Jan 2014 17:43:25 GMT; Path=/")
    test "missing value" $ Assert.equal
      (Map.fromFoldable [Tuple "bar" "1", Tuple "fizz" "", Tuple "buzz" "2"])
      (Cookies.parseCookieHeader "foo; bar=1; fizz= ; buzz=2")
    test "assigns only once" $ Assert.equal
      (Map.fromFoldable [Tuple "foo" "%1", Tuple "bar" "bar"])
      (Cookies.parseCookieHeader "foo=%1;bar=bar;foo=boo")
    test "assigns only once #2" $ Assert.equal
      (Map.fromFoldable [Tuple "foo" "false", Tuple "bar" "bar"])
      (Cookies.parseCookieHeader "foo=false;bar=bar;foo=true")
    test "assigns only once #3" $ Assert.equal
      (Map.fromFoldable [Tuple "foo" "", Tuple "bar" "bar"])
      (Cookies.parseCookieHeader "foo=;bar=bar;foo=boo")
