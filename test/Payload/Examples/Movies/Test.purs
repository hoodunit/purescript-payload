module Payload.Examples.Movies.Test where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Payload.Client (mkGuardedClient)
import Payload.Client as Client
import Payload.Examples.Movies.Main (moviesApi, moviesApiSpec)
import Payload.Headers as Headers
import Payload.Server.Cookies as Cookies
import Payload.Test.Config (TestConfig)
import Payload.Test.Helpers (assertFail, bodyEquals, withServer)
import Test.Unit (TestSuite, suite, test)

cookieOpts :: Map String String -> Client.RequestOptions
cookieOpts cookies = { extraHeaders: Headers.fromFoldable [cookieHeader] }
  where
    cookieHeader = case Cookies.cookieHeader cookies of
                     Tuple field value -> Tuple field value
  
tests :: TestConfig -> TestSuite
tests cfg = do
  let client = mkGuardedClient cfg.clientOpts moviesApiSpec
  let withApi = withServer moviesApiSpec moviesApi
  suite "Example: movies API" do
    test "Sub-route fails if parent route guard fails (missing API key)" $ do
      withApi do
        assertFail (client.v1.movies.latest {})
    test "Sub-route succeeds if parent route guard succeeds (has API key)" $ do
      withApi do
        let opts = cookieOpts (Map.singleton "apiKey" "key")
        res <- client.v1.movies.latest_ opts {}
        bodyEquals { id: 723, title: "The Godfather" } res
    test "Sub-route fails if passes parent guard but not child guard (missing session key)" $ do
      withApi do
        let payload = { params: { movieId: 1 }, body: { value: 9.0 } }
        let opts = cookieOpts $ Map.singleton "apiKey" "key"
        assertFail $ client.v1.movies.byId.rating.create_ opts payload
    test "Sub-route succeeds if passes parent and child guards (has API and session keys)" $ do
      withApi do
        let opts = cookieOpts $ Map.fromFoldable [Tuple "apiKey" "key", Tuple "sessionId" "sessionId"]
        res <- client.v1.movies.byId.rating.create_ opts { params: { movieId: 1 }, body: { value: 9.0 } }
        bodyEquals { statusCode: 1, statusMessage: "Created" } res
