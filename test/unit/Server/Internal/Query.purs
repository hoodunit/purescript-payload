module Payload.Test.Unit.Server.Internal.Query where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Payload.Server.Internal.Query as Query
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))

tests :: TestSuite
tests = do
  suite "Query parameter decoding" do
    suite "Single match" do
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

    suite "Multi-match" do
      test "decoding multi-match" do
        Assert.equal
          (Right {all: Object.fromFoldable [ Tuple "foo" ["blah"]
                                           , Tuple "limit" ["12"]
                                           , Tuple "a" ["b"] ]})
            (Query.decodeQuery
              (SProxy :: SProxy "/search?<..all>")
              (Proxy :: Proxy { all :: Object (Array String) })
              "foo=blah&limit=12&a=b")
      test "removes other matches from multi-matched result" do
        Assert.equal
          (Right { foo: "blah"
                 , bar: "bar"
                 , all: Object.fromFoldable [ Tuple "limit" ["12"], Tuple "a" ["b"] ]})
            (Query.decodeQuery
              (SProxy :: SProxy "/search?foo=<foo>&bar=<bar>&<..all>")
              (Proxy :: Proxy { foo :: String, bar :: String, all :: Object (Array String) })
              "foo=blah&limit=12&a=b&bar=bar")
