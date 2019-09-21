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
import Payload.Response (Response(..))
import Payload.Response as Response
import Payload.Routable (API(..))
import Payload.Route (GET, POST, HEAD)
import Payload.Status as Status
import Payload.Test.Helpers (withServer)
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy

newtype User = User
  { id :: Int
  , name :: String }

api :: API
  { guards :: {}
  , routes ::
    { foo :: GET "/foo"
      { response :: String }
    , fooPost :: POST "/foo"
      { body :: { message :: String }
      , response :: String }
    , fooHead :: HEAD "/fooHead"
      { response :: Unit }
    , bar :: GET "/bar"
      { response :: String }
    , barHead :: HEAD "/bar"
      { response :: Unit }
    }}
api = API

foo :: {} -> Aff String
foo _ = pure "Response"

bar :: {} -> Aff String
bar _ = pure "bar"

barHead :: {} -> Aff (Response Unit)
barHead _ = pure (Response.status Status.accepted unit)

fooPost :: { body :: { message :: String } } -> Aff String
fooPost { body: { message } } = pure $ "Received '" <> message <> "'"

fooHead :: {} -> Aff Unit
fooHead _ = pure unit

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
  suite "Methods" do
    suite "GET" do
      test "GET succeeds" $ do
        res <- get "/foo"
        Assert.equal { status: 200, body: "Response" } res
    suite "POST" do
      test "POST succeeds" $ do
        res <- post "/foo" "{ \"message\": \"Hi there\" }"
        Assert.equal { status: 200, body: "Received 'Hi there'" } res
    suite "HEAD" do
      test "HEAD succeeds" $ do
        res <- head "/fooHead"
        Assert.equal { status: 200, body: "" } res
      test "HEAD is accepted where GET route is defined" $ do
        res <- head "/foo"
        Assert.equal { status: 200, body: "" } res
      test "user-specified HEAD route overrides default GET HEAD route" $ do
        res <- head "/bar"
        Assert.equal { status: 202, body: "" } res

runTests :: Aff Unit
runTests = do
  let handlers = { foo, fooPost, fooHead, bar, barHead }
  withServer api { guards: {}, handlers } (runTestWith Fancy.runTest tests)
