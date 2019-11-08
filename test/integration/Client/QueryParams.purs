module Payload.Test.Integration.Client.QueryParams where

import Prelude

import Data.Either (Either(..))
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
        Assert.equal (Right "Response") res
    test "succeeds with query key" $ do
      let spec = Spec :: _ { foo :: GET "/foo?secret=<secret>"
                                     { query :: { secret :: String }
                                     , response :: String } }
      let handlers = { foo: \{ secret } -> pure secret }
      withRoutes spec handlers do
        let client = mkClient cfg.clientOpts spec
        res <- client.foo identity { secret: "something" }
        Assert.equal res (Right "something")
    -- test "succeeds with multimatch" $ do
    --   let spec = Spec :: _ { foo :: GET "/foo?<..any>"
    --                                  { query :: { secret :: String }
    --                                  , response :: String } }
    --   let handlers = { foo: \{ secret } -> pure secret }
    --   withRoutes spec handlers do
    --     let client = mkClient cfg.clientOpts spec
    --     res <- client.foo identity { secret: "something" }
    --     Assert.equal res (Right "something")
