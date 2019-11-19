module Payload.Test.Integration.Client.Methods where

import Prelude

import Data.Either (Either(..))
import Payload.Client.Client (mkClient)
import Payload.ResponseTypes (Empty(..))
import Payload.Server.Response as Response
import Payload.Spec (DELETE, GET, HEAD, POST, PUT, Routes, Spec(Spec))
import Payload.Test.Config (TestConfig)
import Payload.Test.Helpers (withRoutes)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
  
tests :: TestConfig -> TestSuite
tests cfg = do
  suite "Methods" do
    suite "GET" do
      test "GET succeeds" $ do
        let spec = Spec :: _ { foo :: GET "/foo"
                                       { response :: String } }
        let handlers = { foo: \_ -> pure "Response" }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          Assert.equal res (Right "Response")
      test "GET decodes Int response" $ do
        let spec = Spec :: _ { foo :: GET "/foo"
                                       { response :: { foo :: Int } } }
        let handlers = { foo: \_ -> pure { foo: 12 } }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          Assert.equal (Right { foo: 12 }) res
      test "GET succeeds with URL params" $ do
        let spec = Spec :: _ { foo :: GET "/foo/<id>/<thing>"
                                       { params :: { id :: Int, thing :: String }
                                       , response :: String } }
        let handlers = { foo: \{params: {id, thing}} -> pure $ "ID " <> show (id :: Int) <> ", " <> thing }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { params: { id: 1, thing: "hey" } }
          Assert.equal res (Right "ID 1, hey")

    suite "POST" do
      test "POST succeeds" $ do
        let spec = Spec :: _ { foo :: POST "/foo"
                                       { body :: { message :: String }
                                       , response :: String } }
        let handlers = { foo: \({body: {message}}) -> pure $ "Received '" <> message <> "'" }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { body: { message: "Hi there" } }
          Assert.equal res (Right "Received 'Hi there'")
      test "POST succeeds with empty body route" $ do
        let spec = Spec :: _ { foo :: POST "/foo"
                                       { body :: String
                                       , response :: String } }
        let handlers = { foo: \_ -> pure $ "fooEmpty" }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { body: "" }
          Assert.equal res (Right "fooEmpty")

    suite "HEAD" do
      test "HEAD succeeds" $ do
        let spec = Spec :: _ { foo :: HEAD "/foo" {} }
        let handlers = { foo: \_ -> pure Empty }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          Assert.equal (Right "") res

    suite "PUT" do
      test "PUT succeeds without body" $ do
        let spec = Spec :: _ { foo :: PUT "/foo" { response :: String } }
        let handlers = { foo: \_ -> pure "Put" }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          Assert.equal (Right "Put") res
      test "PUT succeeds with body" $ do
        let spec = Spec :: _ { foo :: PUT "/foo" { body :: String, response :: String } }
        let handlers = { foo: \{ body } -> pure body }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { body: "Put!" }
          Assert.equal (Right "Put!") res

    suite "DELETE" do
      test "DELETE succeeds without body" $ do
        let spec = Spec :: _ { foo :: DELETE "/foo" { response :: String } }
        let handlers = { foo: \_ -> pure "Delete" }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          Assert.equal (Right "Delete") res
      test "DELETE succeeds with String body" $ do
        let spec = Spec :: _ { foo :: DELETE "/foo" { body :: String, response :: String } }
        let handlers = { foo: \{body} -> pure body }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { body: "body" }
          Assert.equal (Right "body") res
      test "DELETE succeeds with body if defined in spec" $ do
        let spec = Spec :: _ { foo :: DELETE "/foo" { body :: Array Int, response :: String } }
        let handlers = { foo: \{ body } -> pure (show body) }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { body: [1] }
          Assert.equal (Right "[1]") res
      test "DELETE succeeds with params" $ do
        let spec = Spec :: _ { foo :: DELETE "/foo/<id>" { params :: { id :: Int }, response :: String } }
        let handlers = { foo: \{params: {id}} -> pure $ "Delete " <> show id }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { params: { id: 1 } }
          Assert.equal (Right "Delete 1") res
      test "DELETE succeeds with nested route" $ do
        let spec = Spec :: _ { v1 :: Routes "/v1" { foo :: DELETE "/foo" { body :: Array Int, response :: String } } }
        let handlers = { v1: { foo: ((\{body} -> pure (show body) )) } }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.v1.foo { body: [1] }
          Assert.equal (Right "[1]") res
