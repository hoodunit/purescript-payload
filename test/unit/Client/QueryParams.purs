module Payload.Test.Unit.Client.QueryParams where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Payload.Client.QueryParams (encodeQueryParam, encodeQueryParamMulti)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "query param encoding" do
  suite "EncodeQueryParam" do
    suite "String" do
      test "encodes string without special character as is" $ do
        Assert.equal (Just "asdf") (encodeQueryParam "asdf")
      test "URL encodes string with special characters" $ do
        Assert.equal (Just "%C3%A4iti%20ja%20is%C3%A4") (encodeQueryParam "äiti ja isä")
    suite "Boolean" do
      test "encodes true as \"true\"" $ do
        Assert.equal (Just "true") (encodeQueryParam true)
      test "encodes false as \"false\"" $ do
        Assert.equal (Just "false") (encodeQueryParam false)
  suite "EncodeQueryParamMulti" do
    suite "Object (Array String)" do
      test "URL encodes strings with special characters" $ do
        let params = Object.fromFoldable [Tuple "q" ["äiti ja isä"], Tuple "foo" ["asdf"]]
        Assert.equal (Just "q=%C3%A4iti%20ja%20is%C3%A4&foo=asdf") (encodeQueryParamMulti params)
      test "encodes array of values as separate key value pairs" $ do
        let params = Object.fromFoldable [Tuple "foo" ["foo1", "foo2"]]
        Assert.equal (Just "foo=foo1&foo=foo2") (encodeQueryParamMulti params)
