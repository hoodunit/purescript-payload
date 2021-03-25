module Payload.Test.Unit.Server.Response where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.ResponseTypes (Empty(..), Json(..), RawResponse, Response(..), ResponseBody(..))
import Payload.Server.Response (class EncodeResponse, encodeResponse)
import Payload.Server.Response as Response
import Payload.Server.Status as Status
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

_header :: String -> RawResponse -> Either String String
_header key res =
    note ("No header with key '" <> key <> "'") $ Headers.lookup key (unwrap res).headers

_headers :: RawResponse -> Either String Headers
_headers res = Right $ (unwrap res).headers

_status :: RawResponse -> Either String Int
_status res = Right $ (unwrap res).status.code

_body :: RawResponse -> Either String ResponseBody
_body res = Right $ (unwrap res).body

encodeOk :: forall a. EncodeResponse a => a -> Aff (Either String RawResponse)
encodeOk = (map $ lmap show) <<< runExceptT <<< encodeResponse <<< Response.ok

encode :: forall a. EncodeResponse a => Response a -> Aff (Either String RawResponse)
encode = (map $ lmap show) <<< runExceptT <<< encodeResponse

tests :: TestSuite
tests = suite "Response" do
  suite "Responder" do
    suite "string" do
      test "sets status to 200" do
        res <- encodeOk "foo"
        Assert.equal (Right 200) (res >>= _status)
      test "sets content-type to text/plain" do
        res <- encodeOk "foo"
        Assert.equal (Right "text/plain; charset=utf-8") (res >>= _header "content-type")
      test "leaves body untouched" do
        res <- encodeOk "foo"
        Assert.equal (Right (StringBody "foo")) (res >>= _body)
    suite "record (treated as JSON)" do
      test "sets status to 200" do
        res <- encodeOk { id: 1 }
        Assert.equal (Right 200) (res >>= _status)
      test "sets content-type to application/json" do
        res <- encodeOk { id: 1 }
        Assert.equal (Right "application/json") (res >>= _header "content-type")
      test "encodes body" do
        res <- encodeOk { id: 1 }
        Assert.equal (Right (StringBody "{\"id\":1}")) (res >>= _body)
    suite "array (treated as JSON)" do
      test "sets status to 200" do
        res <- encodeOk [1, 2, 3]
        Assert.equal (Right 200) (res >>= _status)
      test "sets content-type to application/json" do
        res <- encodeOk [1, 2, 3]
        Assert.equal (Right "application/json") (res >>= _header "content-type")
      test "encodes body" do
        res <- encodeOk [1, 2, 3]
        Assert.equal (Right (StringBody "[1,2,3]")) (res >>= _body)
    suite "Json" do
      test "sets status to 200" do
        res <- encodeOk (Json [1, 2, 3])
        Assert.equal (Right 200) (res >>= _status)
      test "sets content-type to application/json" do
        res <- encodeOk (Json [1, 2, 3])
        Assert.equal (Right "application/json") (res >>= _header "content-type")
      test "encodes body as JSON" do
        res <- encodeOk (Json [1, 2, 3])
        Assert.equal (Right (StringBody "[1,2,3]")) (res >>= _body)
      test "encodes string body as JSON" do
        res <- encodeOk (Json "hello")
        Assert.equal (Right (StringBody "\"hello\"")) (res >>= _body)
      test "encodes int body as JSON" do
        res <- encodeOk (Json 1)
        Assert.equal (Right (StringBody "1")) (res >>= _body)
    suite "Maybe" do
      test "returns 404 if response is Nothing" do
        res <- encodeOk (Nothing :: Maybe String)
        Assert.equal (Right 404) (res >>= _status)
      test "returns empty body if response is Nothing" do
        res <- encodeOk (Nothing :: Maybe String)
        Assert.equal (Right EmptyBody) (res >>= _body)
      test "returns inner response status if response is Just" do
        res <- encodeOk (Just {foo: 1})
        Assert.equal (Right 200) (res >>= _status)
      test "returns inner response body if response is Just" do
        res <- encodeOk (Just {foo: 1})
        Assert.equal (Right (StringBody "{\"foo\":1}")) (res >>= _body)
    suite "Empty" do
      test "returns 200" do
        res <- encodeOk Empty
        Assert.equal (Right 200) (res >>= _status)
      test "returns no headers" do
        res <- encodeOk Empty
        Assert.equal (Right Headers.empty) (res >>= _headers)
    suite "Response" do
      test "returning response with raw String ResponseBody leaves response untouched" do
        let rawRes = Response {status: Status.accepted, headers: Headers.empty, body: StringBody "foo"}
        res <- encode rawRes
        Assert.equal (Right rawRes) res
      suite "adding/overriding headers" do
        test "setting new headers in own response -> default header is added to new headers" do
          let headers = Headers.fromFoldable [Tuple "foo" "fooVal"]
          let expected = Headers.fromFoldable [Tuple "foo" "fooVal", Tuple "content-type" "text/plain; charset=utf-8"]
          res <- encode (Response { status: Status.ok, headers, body: "foo" })
          Assert.equal (Right expected) (res >>= _headers)
        test "setting same header in own response -> default header is not added" do
          let headers = Headers.fromFoldable [Tuple "content-type" "magic"]
          res <- encode (Response { status: Status.ok, headers, body: "foo" })
          Assert.equal (Right headers) (res >>= _headers)
        test "setting same header in different case in own response -> default header is not added" do
          let headers = Headers.fromFoldable [Tuple "cOnTeNt-TyPe" "magic"]
          let expected = Headers.fromFoldable [Tuple "content-type" "magic"]
          res <- encode (Response { status: Status.ok, headers, body: "foo" })
          Assert.equal (Right headers) (res >>= _headers)
        test "added header names are converted to lower case" do
          let headers = Headers.fromFoldable [Tuple "FOO" "fooVal"]
          let expected = Headers.fromFoldable [Tuple "foo" "fooVal"]
          res <- encode (Response { status: Status.ok, headers, body: StringBody "foo" })
          Assert.equal (Right expected) (res >>= _headers)
