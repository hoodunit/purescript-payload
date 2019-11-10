module Payload.Test.Integration.Client.QueryParams where

import Prelude

import Data.Either (Either(..))
import Data.List (List)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Payload.Client.Client (mkClient)
import Payload.Spec (GET, Spec(Spec))
import Payload.Test.Config (TestConfig)
import Payload.Test.Helpers (withRoutes)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
  
tests :: TestConfig -> TestSuite
tests cfg = do
  suite "QueryParams" do
    test "succeeds with query literal" $ do
      let spec = Spec :: _ { foo :: GET "/foo?secret"
                                     { response :: String } }
      let handlers = { foo: \_ -> pure "Response" }
      withRoutes spec handlers do
        let client = mkClient cfg.clientOpts spec
        res <- client.foo identity {}
        Assert.equal res (Right "Response")
    test "succeeds with query key" $ do
      let spec = Spec :: _ { foo :: GET "/foo?secret=<secret>"
                                     { query :: { secret :: String }
                                     , response :: String } }
      let handlers = { foo: \{ secret } -> pure secret }
      withRoutes spec handlers do
        let client = mkClient cfg.clientOpts spec
        res <- client.foo identity { secret: "something" }
        Assert.equal (Right "something") res
    test "succeeds with multimatch" $ do
      let spec = Spec :: _ { foo :: GET "/foo?<..any>"
                                     { query :: { any :: Object String }
                                     , response :: String } }
      let handlers = { foo: \{ any } -> pure (show any) }
      withRoutes spec handlers do
        let client = mkClient cfg.clientOpts spec
        res <- client.foo identity { any: Object.fromFoldable [Tuple "foo" "foo1", Tuple "bar" "bar1"] }
        let expected = (Right "(fromFoldable [(Tuple \"foo\" \"\\\"foo1\\\"\"),(Tuple \"bar\" \"\\\"bar1\\\"\")])")
        Assert.equal expected res
