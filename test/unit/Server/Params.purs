module Payload.Test.Unit.Server.Params where

import Prelude

import Data.Either (Either(..))
import Payload.Server.Params (decodeParam)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  suite "Parameter decoding: decodeParam" do
    test "decodes string" do
      Assert.equal (Right "foo") (decodeParam "foo")
    test "decodes integer" do
      Assert.equal (Right 12) (decodeParam "12")
