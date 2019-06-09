module Payload.Test.Response where

import Prelude

import Data.Either (Either(..), note)
import Data.Map as Map
import Data.Newtype (unwrap)
import Payload.Response (RawResponse(..), mkResponse)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

_header :: forall r. String -> RawResponse r -> Either String String
_header key res = do
  let headers = (unwrap res).headers
  note ("No header with key '" <> key <> "'") $ Map.lookup key headers

_status :: forall r. RawResponse r -> Either String Int
_status res = Right $ (unwrap res).status

tests :: TestSuite
tests = suite "Response" do
  suite "Responder" do
    suite "string" do
      test "sets status to 200" do
        res <- mkResponse "foo"
        Assert.equal (Right 200) (res >>= _status)
      test "sets Content-Type to text/plain" do
        res <- mkResponse "foo"
        Assert.equal (Right "text/plain") (res >>= _header "Content-Type")
