module Payload.Test.Unit.Server.Internal.Url where

import Prelude

import Data.Either (Either(..), isLeft)
import Payload.Server.Internal.Url as Url
import Payload.Server.Internal.UrlString (splitRequestTarget, urlToSegments)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))

tests :: TestSuite
tests = suite "URL value decoding" do
  suite "decodeUrl: decoding query URLs" do
    suite "URL parameters" do
      test "decoding int succeeds for valid int" do
        Assert.equal
          (Right {id: 12})
          (Url.decodeUrl
            (Proxy :: Proxy "/users/<id>")
            (Proxy :: Proxy { id :: Int })
            (urlToSegments "/users/12"))
      test "decoding int fails for number" do
        Assert.assert "Expected error"
          (isLeft
            (Url.decodeUrl
              (Proxy :: Proxy "/users/<id>")
              (Proxy :: Proxy { id :: Int })
              (urlToSegments "/users/12.1")))
      test "decoding int fails for string" do
        Assert.assert "Expected error"
          (isLeft
            (Url.decodeUrl
              (Proxy :: Proxy "/users/<id>")
              (Proxy :: Proxy { id :: Int })
              (urlToSegments "/users/asdf")))
      test "decoding multiple items succeeds" do
        Assert.equal
          (Right {userId: 12, postId: 23})
          (Url.decodeUrl
            (Proxy :: Proxy "/users/<userId>/posts/<postId>")
            (Proxy :: Proxy { userId :: Int, postId :: Int })
            (urlToSegments "/users/12/posts/23"))
  suite "splitRequestTarget: reading the path and query of a request target" do
    test "the query is passed on raw, non-ASCII included" do
      Assert.equal { path: "/q", query: "name=é&x='a'" } (splitRequestTarget "/q?name=é&x='a'")
    test "the query starts after the first ?" do
      Assert.equal { path: "/a", query: "b?c" } (splitRequestTarget "/a?b?c")
    test "a fragment is dropped" do
      Assert.equal { path: "/a", query: "b" } (splitRequestTarget "/a?b#c")
    test "an absolute-form target is read from its path" do
      Assert.equal { path: "/p", query: "q" } (splitRequestTarget "http://host:1/p?q")
