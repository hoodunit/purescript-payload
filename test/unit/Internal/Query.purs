module Payload.Test.Unit.Internal.Query where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Symbol (SProxy(..))
import Payload.Internal.Query as Query
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))

tests :: TestSuite
tests = do
  suite "Query parameter decoding" do
    test "decoding int succeeds for valid int" do
      Assert.equal
        (Right {limit: 12})
          (Query.decodeQuery
            (SProxy :: SProxy "/search?limit=<limit>")
            (Proxy :: Proxy { limit :: Int })
            "limit=12")
    test "decoding int fails for invalid int" do
      Assert.equal
        true
        (isLeft
          (Query.decodeQuery
            (SProxy :: SProxy "/search?limit=<limit>")
            (Proxy :: Proxy { limit :: Int })
            "limit=asdf"))
    test "decoding string succeeds" do
      Assert.equal
        (Right {query: "whatever"})
          (Query.decodeQuery
            (SProxy :: SProxy "/search?query=<query>")
            (Proxy :: Proxy { query :: String })
            "query=whatever")
    test "extra parameters are ignored" do
      Assert.equal
        (Right {limit: 12})
          (Query.decodeQuery
            (SProxy :: SProxy "/search?limit=<limit>")
            (Proxy :: Proxy { limit :: Int })
            "foo=blah&limit=12&a=b")
