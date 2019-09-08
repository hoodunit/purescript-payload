module Payload.Test.Query where

import Prelude

import Affjax as AX
import Affjax.RequestBody as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, isLeft)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Payload.Query as Query
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
    { search :: GET "/search?limit=<limit>"
      { query :: { limit :: Int }
      , response :: String }
    , profile :: POST "/profile?id=<id>&foo=<foo>"
      { query :: { id :: Int, foo :: String }
      , body :: String
      , response :: String }}}
api = API

search :: { limit :: Int } -> Aff String
search _ = pure "Search result"

profile :: { id :: Int, foo :: String, body :: String } -> Aff String
profile _ = pure "Saved profile"

data Method = GET | POST

type ApiResponse =
  { status :: Int
  , body :: String }

getRequest :: String -> String -> Aff ApiResponse
getRequest host path = do
  res <- AX.get ResponseFormat.string (host <> "/" <> path)
  let body = either ResponseFormat.printResponseFormatError identity res.body
  pure { status: unwrapStatusCode res.status, body }

postRequest :: String -> String -> String -> Aff ApiResponse
postRequest host path body = do
  res <- AX.post ResponseFormat.string ("http://localhost:3000/" <> path) (RequestBody.String body)
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
  suite "Query requests" do
    suite "keys (foo=<myFoo>)" do
      test "GET /search?limit=3 succeeds" $ do
        res <- get "/search?limit=3"
        Assert.equal { status: 200, body: "Search result" } res
      test "GET /search?limit=3.1 fails (not an integer)" $ do
        res <- get "/search?limit=3.1"
        Assert.equal { status: 404, body: "" } res
      test "GET /search?limit=asdf fails (not an integer)" $ do
        res <- get "/search?limit=asdf"
        Assert.equal { status: 404, body: "" } res
      test "POST /profile?id=3&foo=asdf succeeds" $ do
        res <- post "/profile?id=3&foo=asdf" ""
        Assert.equal { status: 200, body: "Saved profile" } res
      test "POST /profile?id=3.0&foo=asdf fails" $ do
        res <- post "/profile?id=3.0&foo=asdf" ""
        Assert.equal { status: 404, body: "" } res
  suite "Query decoding" do
    test "decoding int succeeds for valid int" do
      Assert.equal
        (Right {limit: 12})
          (Query.decodeQuery
            (SProxy :: SProxy "/search?limit=<limit>")
            (Proxy :: Proxy { limit :: Int })
            "limit=12")
    test "decoding int fails for invalid int" do
      Assert.equal
        true
        (isLeft
          (Query.decodeQuery
            (SProxy :: SProxy "/search?limit=<limit>")
            (Proxy :: Proxy { limit :: Int })
            "limit=asdf"))
    test "decoding string succeeds" do
      Assert.equal
        (Right {query: "whatever"})
          (Query.decodeQuery
            (SProxy :: SProxy "/search?query=<query>")
            (Proxy :: Proxy { query :: String })
            "query=whatever")
    test "extra parameters are ignored" do
      Assert.equal
        (Right {limit: 12})
          (Query.decodeQuery
            (SProxy :: SProxy "/search?limit=<limit>")
            (Proxy :: Proxy { limit :: Int })
            "foo=blah&limit=12&a=b")

runTests :: Aff Unit
runTests = do
  let guards = {}
  let handlers = { search, profile }
  withServer api { guards, handlers } (runTestWith Fancy.runTest tests)
