module Payload.Test.Integration.Client.Methods where

import Prelude

import Payload.Client (mkClient)
import Payload.ResponseTypes (Empty(..))
import Payload.Spec (DELETE, GET, HEAD, POST, PUT, Routes, Spec(Spec))
import Payload.Test.Config (TestConfig)
import Payload.Test.Helpers (bodyEquals, withRoutes)
import Test.Unit (TestSuite, suite, test)
  
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
          bodyEquals "Response" res
      test "GET decodes Int response" $ do
        let spec = Spec :: _ { foo :: GET "/foo"
                                       { response :: { foo :: Int } } }
        let handlers = { foo: \_ -> pure { foo: 12 } }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          bodyEquals { foo: 12 } res
      test "GET succeeds with URL params" $ do
        let spec = Spec :: _ { foo :: GET "/foo/{id}/{thing}"
                                       { params :: { id :: Int, thing :: String }
                                       , response :: String } }
        let handlers = { foo: \{params: {id, thing}} -> pure $ "ID " <> show (id :: Int) <> ", " <> thing }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { params: { id: 1, thing: "hey" } }
          bodyEquals "ID 1, hey" res

    suite "POST" do
      test "POST succeeds" $ do
        let spec = Spec :: _ { foo :: POST "/foo"
                                       { body :: { message :: String }
                                       , response :: String } }
        let handlers = { foo: \({body: {message}}) -> pure $ "Received '" <> message <> "'" }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { body: { message: "Hi there" } }
          bodyEquals "Received 'Hi there'" res
      test "POST succeeds with empty body route" $ do
        let spec = Spec :: _ { foo :: POST "/foo"
                                       { body :: String
                                       , response :: String } }
        let handlers = { foo: \_ -> pure $ "fooEmpty" }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { body: "" }
          bodyEquals "fooEmpty" res

    suite "HEAD" do
      test "HEAD succeeds" $ do
        let spec = Spec :: _ { foo :: HEAD "/foo" {} }
        let handlers = { foo: \_ -> pure Empty }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          bodyEquals "" res

    suite "PUT" do
      test "PUT succeeds without body" $ do
        let spec = Spec :: _ { foo :: PUT "/foo" { response :: String } }
        let handlers = { foo: \_ -> pure "Put" }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          bodyEquals "Put" res
      test "PUT succeeds with body" $ do
        let spec = Spec :: _ { foo :: PUT "/foo" { body :: String, response :: String } }
        let handlers = { foo: \{ body } -> pure body }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { body: "Put!" }
          bodyEquals "Put!" res

    suite "DELETE" do
      test "DELETE succeeds without body" $ do
        let spec = Spec :: _ { foo :: DELETE "/foo" { response :: String } }
        let handlers = { foo: \_ -> pure "Delete" }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo {}
          bodyEquals "Delete" res
      test "DELETE succeeds with String body" $ do
        let spec = Spec :: _ { foo :: DELETE "/foo" { body :: String, response :: String } }
        let handlers = { foo: \{body} -> pure body }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { body: "body" }
          bodyEquals "body" res
      test "DELETE succeeds with body if defined in spec" $ do
        let spec = Spec :: _ { foo :: DELETE "/foo" { body :: Array Int, response :: String } }
        let handlers = { foo: \{ body } -> pure (show body) }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { body: [1] }
          bodyEquals "[1]" res
      test "DELETE succeeds with params" $ do
        let spec = Spec :: _ { foo :: DELETE "/foo/{id}" { params :: { id :: Int }, response :: String } }
        let handlers = { foo: \{params: {id}} -> pure $ "Delete " <> show id }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.foo { params: { id: 1 } }
          bodyEquals "Delete 1" res
      test "DELETE succeeds with nested route" $ do
        let spec = Spec :: _ { v1 :: Routes "/v1" { foo :: DELETE "/foo" { body :: Array Int, response :: String } } }
        let handlers = { v1: { foo: ((\{body} -> pure (show body) )) } }
        withRoutes spec handlers do
          let client = mkClient cfg.clientOpts spec
          res <- client.v1.foo { body: [1] }
          bodyEquals "[1]" res
