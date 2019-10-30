module Payload.Examples.Movies.Test where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Node.HTTP as HTTP
import Payload.Client.Client (mkClient, mkGuardedClient)
import Payload.Client.Client as Client
import Payload.Cookies (requestCookies)
import Payload.Cookies as Cookies
import Payload.Examples.Movies.Main (moviesApi, moviesApiSpec)
import Payload.Spec (type (:), Spec(Spec), DELETE, GET, Guards(..), POST, Route, Routes, Nil)
import Payload.Test.Helpers (assertFail, assertRes, withServer)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy

withCookies :: Map String String -> Client.ModifyRequest
withCookies cookies req = req { withCredentials = true, headers = req.headers <> [ cookieHeader ] }
  where
    cookieHeader = case Cookies.cookieHeader cookies of
                     Tuple field value -> RequestHeader field value
  
tests :: TestSuite
tests = do
  let client = mkGuardedClient Client.defaultOpts moviesApiSpec
  let withApi = withServer moviesApiSpec moviesApi
  suite "Example: movies API" do
    test "Sub-route fails if parent route guard fails (missing API key)" $ do
      withApi do
        assertFail (client.v1.movies.latest identity {})
        -- assertFail (client.v1.auth.session.delete identity { body: { sessionId: "blah" }})
    test "Sub-route succeeds if parent route guard succeeds (has API key)" $ do
      withApi do
        assertRes (client.v1.movies.latest (withCookies (Map.singleton "apiKey" "key")) {})
                 { id: 723, title: "The Godfather" }
    test "Sub-route fails if passes parent guard but not child guard (missing session key)" $ do
      withApi do
        let payload = { movieId: 1, body: { value: 9.0 } }
        let opts = withCookies $ Map.singleton "apiKey" "key"
        assertFail $ client.v1.movies.byId.rating.create opts payload
    test "Sub-route succeeds if passes parent and child guards (has API and session keys)" $ do
      withApi do
        let opts = withCookies $ Map.fromFoldable [Tuple "apiKey" "key", Tuple "sessionId" "sessionId"]
        assertRes (client.v1.movies.byId.rating.create opts { movieId: 1, body: { value: 9.0 } })
                   { statusCode: 1, statusMessage: "Created" }
