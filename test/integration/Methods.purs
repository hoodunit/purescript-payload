module Payload.Test.Integration.Methods where

import Prelude

import Affjax as AX
import Affjax.RequestBody as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..), either)
import Effect.Aff (Aff)
import Payload.Route (GET, POST)
import Payload.Routing (API(..))
import Payload.Test.Helpers (withServer)
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy
import Type.Proxy (Proxy(..))

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
      }}
api = API

foo :: {} -> Aff String
foo _ = pure "Response"

fooPost :: { body :: { message :: String } } -> Aff String
fooPost { body: { message } } = pure $ "Received '" <> message <> "'"

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
  suite "Methods" do
    suite "GET" do
      test "GET succeeds" $ do
        res <- get "/foo"
        Assert.equal { status: 200, body: "Response" } res
    suite "POST" do
      test "POST succeeds" $ do
        res <- post "/foo" "{ \"message\": \"Hi there\" }"
        Assert.equal { status: 200, body: "Received 'Hi there'" } res

runTests :: Aff Unit
runTests = do
  let guards = {}
  let handlers = { foo, fooPost }
  withServer api { guards, handlers } (runTestWith Fancy.runTest tests)
