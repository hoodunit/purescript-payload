module Payload.Test.Url where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Payload.Url as Url
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))

tests :: TestSuite
tests = suite "URL type-level parsing" do
  suite "URL decoding: decodeUrl" do
    test "/users/<userId>/posts/<postId> match1" do
      Assert.equal
        (Right {userId: 12, postId: 23})
        (Url.decodeUrl
          (SProxy :: SProxy "/users/<userId>/posts/<postId>")
          (Proxy :: Proxy { userId :: Int, postId :: Int })
          ("users" : "12" : "posts" : "23" : Nil))
    test "/users/<userId>/posts/<postId> match2" do
      Assert.equal
        (Right {userId: "12", postId: 23})
        (Url.decodeUrl
          (SProxy :: SProxy "/users/<userId>/posts/<postId>")
          (Proxy :: Proxy { userId :: String, postId :: Int })
          ("users" : "12" : "posts" : "23" : Nil))
