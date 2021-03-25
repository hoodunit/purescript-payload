module Payload.Test.Integration.Client.ContentTypes where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Payload.Client (mkGuardedClient)
import Payload.ContentType as ContentType
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.Server.Guards as Guards
import Payload.Spec (type (:), GET, Guards, Nil, POST, PUT, Spec(..), DELETE)
import Payload.Test.Config (TestConfig)
import Payload.Test.Helpers (bodyEquals, withServer)
import Test.Unit (TestSuite, suite, test)

checkContentType :: forall r. {guards :: {headers :: Headers} | r} -> Aff String
checkContentType {guards: {headers}} = case Headers.lookup "content-type" headers of
  Just contentType -> pure contentType
  Nothing -> pure "no content-type"
  
tests :: TestConfig -> TestSuite
tests cfg = do
  suite "Content Types" do
    test "GET does not add content-type header" do
      let spec = Spec :: _ {
                             guards :: { headers :: Headers }
                             , routes :: {
                                 foo :: GET "/foo" {
                                    guards :: Guards ("headers" : Nil),
                                    response :: String
                                 }
                             }
                           }
      let api = { guards: { headers: Guards.headers }, handlers: { foo: checkContentType } }
      withServer spec api do
        let client = mkGuardedClient cfg.clientOpts spec
        res <- client.foo {}
        bodyEquals "no content-type" res
    test "POST adds content-type header" do
      let spec = Spec :: _ {
                             guards :: { headers :: Headers }
                             , routes :: {
                                 foo :: POST "/foo" {
                                    guards :: Guards ("headers" : Nil),
                                    body :: String,
                                    response :: String
                                 }
                             }
                           }
      let api = { guards: { headers: Guards.headers }, handlers: { foo: checkContentType } }
      withServer spec api do
        let client = mkGuardedClient cfg.clientOpts spec
        res <- client.foo { body: "Hello!" }
        bodyEquals ContentType.plain res
    test "PUT adds content-type if body is provided" do
      let spec = Spec :: _ {
                             guards :: { headers :: Headers }
                             , routes :: {
                                 foo :: PUT "/foo" {
                                    guards :: Guards ("headers" : Nil),
                                    body :: String,
                                    response :: String
                                 }
                             }
                           }
      let api = { guards: { headers: Guards.headers }, handlers: { foo: checkContentType } }
      withServer spec api do
        let client = mkGuardedClient cfg.clientOpts spec
        res <- client.foo {body: "hello"}
        bodyEquals ContentType.plain res
    test "PUT omits content-type if body is not provided" do
      let spec = Spec :: _ {
                             guards :: { headers :: Headers }
                             , routes :: {
                                 foo :: PUT "/foo" {
                                    guards :: Guards ("headers" : Nil),
                                    response :: String
                                 }
                             }
                           }
      let api = { guards: { headers: Guards.headers }, handlers: { foo: checkContentType } }
      withServer spec api do
        let client = mkGuardedClient cfg.clientOpts spec
        res <- client.foo {}
        bodyEquals "no content-type" res
    test "DELETE adds content-type if body is provided" do
      let spec = Spec :: _ {
                             guards :: { headers :: Headers }
                             , routes :: {
                                 foo :: DELETE "/foo" {
                                    guards :: Guards ("headers" : Nil),
                                    body :: String,
                                    response :: String
                                 }
                             }
                           }
      let api = { guards: { headers: Guards.headers }, handlers: { foo: checkContentType } }
      withServer spec api do
        let client = mkGuardedClient cfg.clientOpts spec
        res <- client.foo {body: "hello"}
        bodyEquals ContentType.plain res
    test "DELETE omits content-type if body is not provided" do
      let spec = Spec :: _ {
                             guards :: { headers :: Headers }
                             , routes :: {
                                 foo :: DELETE "/foo" {
                                    guards :: Guards ("headers" : Nil),
                                    response :: String
                                 }
                             }
                           }
      let api = { guards: { headers: Guards.headers }, handlers: { foo: checkContentType } }
      withServer spec api do
        let client = mkGuardedClient cfg.clientOpts spec
        res <- client.foo {}
        bodyEquals "no content-type" res
