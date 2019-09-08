module Payload.Test.Url where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Payload.Server (urlToSegments)
import Payload.Url as Url
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
            (SProxy :: SProxy "/users/<id>")
            (Proxy :: Proxy { id :: Int })
            (urlToSegments "/users/12"))
      test "decoding int fails for number" do
        Assert.assert "Expected error"
          (isLeft
            (Url.decodeUrl
              (SProxy :: SProxy "/users/<id>")
              (Proxy :: Proxy { id :: Int })
              (urlToSegments "/users/12.1")))
      test "decoding int fails for string" do
        Assert.assert "Expected error"
          (isLeft
            (Url.decodeUrl
              (SProxy :: SProxy "/users/<id>")
              (Proxy :: Proxy { id :: Int })
              (urlToSegments "/users/asdf")))
      test "decoding multiple items succeeds" do
        Assert.equal
          (Right {userId: 12, postId: 23})
          (Url.decodeUrl
            (SProxy :: SProxy "/users/<userId>/posts/<postId>")
            (Proxy :: Proxy { userId :: Int, postId :: Int })
            (urlToSegments "/users/12/posts/23"))
