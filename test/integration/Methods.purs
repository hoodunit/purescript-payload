module Payload.Test.Integration.Methods where

import Prelude

import Affjax as AX
import Affjax.RequestBody as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Response (Response(..), ResponseBody(..))
import Payload.Response as Response
import Payload.Routable (class Routable, API(..))
import Payload.Route (DELETE, GET, HEAD, POST, PUT)
import Payload.Status as Status
import Payload.Test.Helpers (withRoutes, withServer)
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy
import Type.Proxy (Proxy(..))

type ApiResponse =
  { status :: Int
  , body :: String }

getRequest :: String -> String -> Aff ApiResponse
getRequest host path = do
  res <- AX.get ResponseFormat.string (host <> "/" <> path)
  let body = either ResponseFormat.printResponseFormatError identity res.body
  pure { status: unwrapStatusCode res.status, body }

postRequest :: String -> String -> String -> Aff ApiResponse
postRequest host path reqBody = do
  res <- AX.post ResponseFormat.string ("http://localhost:3000/" <> path) (RequestBody.String reqBody)
  let body = either ResponseFormat.printResponseFormatError identity res.body
  pure { status: unwrapStatusCode res.status, body }

putRequest :: String -> String -> String -> Aff ApiResponse
putRequest host path reqBody = do
  res <- AX.put ResponseFormat.string ("http://localhost:3000/" <> path) (RequestBody.String reqBody)
  let body = either ResponseFormat.printResponseFormatError identity res.body
  pure { status: unwrapStatusCode res.status, body }

deleteRequest :: String -> String -> Aff ApiResponse
deleteRequest host path = do
  res <- AX.delete ResponseFormat.string ("http://localhost:3000/" <> path)
  let body = either ResponseFormat.printResponseFormatError identity res.body
  pure { status: unwrapStatusCode res.status, body }

headRequest :: String -> String -> Aff ApiResponse
headRequest host path = do
  let req = AX.defaultRequest
        { method = Left HEAD
        , responseFormat = ResponseFormat.string
        , url = host <> "/" <> path
        }
  res <- AX.request req
  let body = either ResponseFormat.printResponseFormatError identity res.body
  pure { status: unwrapStatusCode res.status, body }

unwrapStatusCode :: StatusCode -> Int
unwrapStatusCode (StatusCode c) = c

assertResp :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertResp req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors
  
tests :: TestSuite
tests = do
  let get = getRequest "http://localhost:3000"
  let post = postRequest "http://localhost:3000"
  let head = headRequest "http://localhost:3000"
  let put = putRequest "http://localhost:3000"
  let delete = deleteRequest "http://localhost:3000"
  suite "Methods" do
    suite "GET" do
      test "GET succeeds" $ do
        let spec = Proxy :: _ { foo :: GET "/foo"
                                       { response :: String } }
        let handlers = { foo: \_ -> pure "Response" }
        withRoutes spec handlers do
          res <- get "/foo"
          Assert.equal { status: 200, body: "Response" } res

    suite "POST" do
      test "POST succeeds" $ do
        let spec = Proxy :: _ { foo :: POST "/foo"
                                       { body :: { message :: String }
                                       , response :: String } }
        let handlers = { foo: \({body: {message}}) -> pure $ "Received '" <> message <> "'" }
        withRoutes spec handlers do
          res <- post "/foo" "{ \"message\": \"Hi there\" }"
          Assert.equal { status: 200, body: "Received 'Hi there'" } res
      test "POST succeeds with empty body route" $ do
        let spec = Proxy :: _ { foo :: POST "/foo"
                                       { body :: String
                                       , response :: String } }
        let handlers = { foo: \_ -> pure $ "fooEmpty" }
        withRoutes spec handlers do
          res <- post "/foo" ""
          Assert.equal { status: 200, body: "fooEmpty" } res

    suite "HEAD" do
      test "HEAD succeeds" $ do
        let spec = Proxy :: _ { foo :: HEAD "/foo" {} }
        let handlers = { foo: \_ -> pure unit }
        withRoutes spec handlers do
          res <- head "/foo"
          Assert.equal { status: 200, body: "" } res
      test "HEAD is accepted where GET route is defined" $ do
        let spec = Proxy :: _ { fooGet :: GET "/foo" { response :: String } }
        let handlers = { fooGet: \_ -> pure "get" }
        withRoutes spec handlers do
          res <- head "/foo"
          Assert.equal { status: 200, body: "" } res
      test "user-specified HEAD route overrides default GET HEAD route" $ do
        let spec = Proxy :: _ { fooGet :: GET "/foo" { response :: String }, fooHead :: HEAD "/foo" {} }
        let handlers = { fooGet: \_ -> pure "get", fooHead: \_ -> pure (Response.status Status.accepted unit) }
        withRoutes spec handlers do
          res <- head "/foo"
          Assert.equal { status: 202, body: "" } res

    suite "PUT" do
      test "PUT succeeds" $ do
        let spec = Proxy :: _ { foo :: PUT "/foo" { response :: String } }
        let handlers = { foo: \_ -> pure "Put" }
        withRoutes spec handlers do
          res <- put "/foo" ""
          Assert.equal { status: 200, body: "Put" } res

    suite "DELETE" do
      test "DELETE succeeds" $ do
        let spec = Proxy :: _ { foo :: DELETE "/foo" { response :: String } }
        let handlers = { foo: \_ -> pure "Delete" }
        withRoutes spec handlers do
          res <- delete "/foo"
          Assert.equal { status: 200, body: "Delete" } res

runTests :: Aff Unit
runTests = do
  runTestWith Fancy.runTest tests

