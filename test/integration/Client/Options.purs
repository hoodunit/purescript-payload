module Payload.Test.Integration.Client.Options where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Payload.Client (mkGuardedClient)
import Payload.Client.Options (defaultReqOpts)
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.Server.Guards as Guards
import Payload.Spec (type (:), GET, Guards, Nil, Spec(..))
import Payload.Test.Config (TestConfig)
import Payload.Test.Helpers (bodyEquals, withServer)
import Test.Unit (TestSuite, suite, test)

getHeader :: forall r. String -> {guards :: {headers :: Headers} | r} -> Aff String
getHeader key {guards: {headers}} = case Headers.lookup key headers of
  Just contentType -> pure contentType
  Nothing -> pure "not found"

tests :: TestConfig -> TestSuite
tests cfg = do
  suite "Client options" do
    suite "extraHeaders" do
      test "adds header to request" do
        let spec = Spec :: _ {
                               guards :: { headers :: Headers }
                               , routes :: {
                                   foo :: GET "/foo" {
                                      guards :: Guards ("headers" : Nil),
                                      response :: String
                                   }
                               }
                             }
        let api = { guards: { headers: Guards.headers }, handlers: { foo: getHeader "accept" } }
        let opts = cfg.clientOpts { extraHeaders = Headers.fromFoldable [Tuple "Accept" "some content type"] }
        withServer spec api do
          let client = mkGuardedClient opts spec
          res <- client.foo {}
          bodyEquals "some content type" res
      test "single endpoint header overrides client extraHeader" do
        let spec = Spec :: _ {
                               guards :: { headers :: Headers }
                               , routes :: {
                                   foo :: GET "/foo" {
                                      guards :: Guards ("headers" : Nil),
                                      response :: String
                                   }
                               }
                             }
        let api = { guards: { headers: Guards.headers }, handlers: { foo: getHeader "accept" } }
        let opts = cfg.clientOpts { extraHeaders = Headers.fromFoldable [Tuple "Accept" "some content type"] }
        withServer spec api do
          let client = mkGuardedClient opts spec
          let reqOpts = defaultReqOpts { extraHeaders = Headers.fromFoldable [Tuple "Accept" "overridden value"] }
          res <- client.foo_ reqOpts {}
          bodyEquals "overridden value" res
