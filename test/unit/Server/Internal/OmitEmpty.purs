module Payload.Test.Unit.Server.Internal.OmitEmpty where

import Prelude

import Payload.Server.Internal.OmitEmpty (omitEmpty)
import Payload.Internal.Route (Undefined(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  suite "OmitEmpty" do
    test "removes empty record field" do
      Assert.equal { foo: { a: "foo" } } (omitEmpty { foo: { a: "foo" }, bar: {} })
    test "removes all empty record fields" do
      Assert.equal
        { foo: { a: "foo" }, qux: { q: "q" } }
        (omitEmpty { foo1: {}, foo: { a: "foo" }, bar: {}, qux: { q: "q" } })
    test "ignores non-record fields" do
      Assert.equal { foo: "a", bar: 1 } (omitEmpty { foo: "a", bar: 1 })
    test "removes Undefined field" do
      Assert.equal {} (omitEmpty { foo: Undefined })
