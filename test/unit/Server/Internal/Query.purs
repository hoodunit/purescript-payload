module Payload.Test.Unit.Server.Internal.Query where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
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
      test "Int: decoding succeeds for valid int" do
        Assert.equal
          (Right {limit: 12})
            (Query.decodeQuery
              (Proxy :: Proxy "/search?limit=<limit>")
              (Proxy :: Proxy { limit :: Int })
              "limit=12")
      test "Int: decoding fails for invalid int" do
        Assert.equal
          true
          (isLeft
            (Query.decodeQuery
              (Proxy :: Proxy "/search?limit=<limit>")
              (Proxy :: Proxy { limit :: Int })
              "limit=asdf"))
      test "String: decoding succeeds" do
        Assert.equal
          (Right {query: "whatever"})
            (Query.decodeQuery
              (Proxy :: Proxy "/search?query=<query>")
              (Proxy :: Proxy { query :: String })
              "query=whatever")
      test "Boolean: \"true\" decodes to true" do
        Assert.equal
          (Right {query: true})
            (Query.decodeQuery
              (Proxy :: Proxy "/search?query=<query>")
              (Proxy :: Proxy { query :: Boolean })
              "query=true")
      test "Boolean: \"false\" decodes to false" do
        Assert.equal
          (Right {query: false})
            (Query.decodeQuery
              (Proxy :: Proxy "/search?query=<query>")
              (Proxy :: Proxy { query :: Boolean })
              "query=false")
      test "Boolean: \"\" fails to decode" do
        Assert.equal
          true
          (isLeft (Query.decodeQuery
            (Proxy :: Proxy "/search?query=<query>")
            (Proxy :: Proxy { query :: Boolean })
            "query="))
      test "Maybe: decoding Maybe Int parses Int when Int is given" do
        Assert.equal
          (Right {query: Just 1})
            (Query.decodeQuery
              (Proxy :: Proxy "/search?query=<query>")
              (Proxy :: Proxy { query :: Maybe Int })
              "query=1")
      test "Maybe: decoding Maybe Int returns Nothing when query value is empty" do
        Assert.equal
          (Right {query: Nothing})
            (Query.decodeQuery
              (Proxy :: Proxy "/search?query=<query>")
              (Proxy :: Proxy { query :: Maybe Int })
              "query=")
      test "Maybe: decoding Maybe Int returns Nothing when query value is omitted" do
        Assert.equal
          (Right {query: Nothing})
            (Query.decodeQuery
              (Proxy :: Proxy "/search?query=<query>")
              (Proxy :: Proxy { query :: Maybe Int })
              "")
      test "extra parameters are ignored" do
        Assert.equal
          (Right {limit: 12})
            (Query.decodeQuery
              (Proxy :: Proxy "/search?limit=<limit>")
              (Proxy :: Proxy { limit :: Int })
              "foo=blah&limit=12&a=b")

    suite "Multi-match" do
      test "decoding multi-match" do
        Assert.equal
          (Right {all: Object.fromFoldable [ Tuple "foo" ["blah"]
                                           , Tuple "limit" ["12"]
                                           , Tuple "a" ["b"] ]})
            (Query.decodeQuery
              (Proxy :: Proxy "/search?<..all>")
              (Proxy :: Proxy { all :: Object (Array String) })
              "foo=blah&limit=12&a=b")
      test "removes other matches from multi-matched result" do
        Assert.equal
          (Right { foo: "blah"
                 , bar: "bar"
                 , all: Object.fromFoldable [ Tuple "limit" ["12"], Tuple "a" ["b"] ]})
            (Query.decodeQuery
              (Proxy :: Proxy "/search?foo=<foo>&bar=<bar>&<..all>")
              (Proxy :: Proxy { foo :: String, bar :: String, all :: Object (Array String) })
              "foo=blah&limit=12&a=b&bar=bar")
