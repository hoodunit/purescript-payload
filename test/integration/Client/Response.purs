module Payload.Test.Integration.Client.Response where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Payload.Client (ClientError(..), ClientResponse, mkClient, unwrapBody)
import Payload.ResponseTypes (Json(..))
import Payload.Server.Response as Response
import Payload.Spec (GET, Spec(Spec))
import Payload.Test.Config (TestConfig)
import Payload.Test.Helpers (bodyEquals, withRoutes)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Web.Streams.ReadableStream (ReadableStream)

foreign import stringsToStream :: Array String -> ReadableStream Uint8Array
foreign import streamToStringImpl :: ReadableStream Uint8Array -> Effect (Promise String)

streamToString :: ReadableStream Uint8Array -> Aff String
streamToString stream = Promise.toAffE (streamToStringImpl stream)

assertErrorRes :: forall body. ClientResponse body -> Aff ClientError
assertErrorRes (Right _) = throwError (error "Expected error")
assertErrorRes (Left err) = pure err

isStatusError :: ClientError -> Boolean
isStatusError (StatusError _) = true
isStatusError _ = false
  
tests :: TestConfig -> TestSuite
tests cfg = do
  suite "Response" do
    test "string" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: String } }
      let handlers = { foo: \_ -> pure "String body" }
      withRoutes spec handlers do
        let client = mkClient cfg.clientOpts spec
        res <- client.foo {}
        bodyEquals "String body" res
    test "record (JSON)" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: { val :: Int } } }
      let handlers = { foo: \_ -> pure { val: 1 } }
      withRoutes spec handlers do
        let client = mkClient cfg.clientOpts spec
        res <- client.foo {}
        bodyEquals { val: 1 } res
    test "array (JSON)" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: Array Int } }
      let handlers = { foo: \_ -> pure [1] }
      withRoutes spec handlers do
        let client = mkClient cfg.clientOpts spec
        res <- client.foo {}
        bodyEquals [1] res
    test "ReadableStream Uint8Array" $ do
      let spec = Spec :: _ { foo :: GET "/foo" { response :: ReadableStream Uint8Array } }
      let handlers = { foo: \_ -> pure (stringsToStream ["a", "s", "d", "f"]) }
      withRoutes spec handlers do
        let client = mkClient cfg.clientOpts spec
        body <- unwrapBody $ client.foo {}
        readStream <- streamToString body
        Assert.equal "asdf" readStream
