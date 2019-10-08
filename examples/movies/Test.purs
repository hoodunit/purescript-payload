module Payload.Examples.Movies.Test where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Data.Either (Either, note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Node.HTTP as HTTP
import Payload.Client.Client (mkClient)
import Payload.Client.Client as Client
import Payload.Cookies (requestCookies)
import Payload.Cookies as Cookies
import Payload.Examples.Movies.Main (moviesApi, moviesApiSpec)
import Payload.Spec (type (:), API(API), DELETE, GET, Guards(..), POST, Route, Routes, Nil)
import Payload.Test.Helpers (assertFail, assertRes, withServer)
import Test.Unit (suite, test)
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy
import Type.Proxy (Proxy(..))

withCookies :: Map String String -> Client.ModifyRequest
withCookies cookies req = req { withCredentials = true, headers = req.headers <> [ cookieHeader ] }
  where
    cookieHeader = case Cookies.cookieHeader cookies of
                     Tuple field value -> RequestHeader field value
  
-- tests :: forall routesSpec client r
--          . ClientApi routesSpec client
--          => client -> TestSuite
tests client = do
  suite "Example: movies API" do
    test "Sub-route fails if parent route guard fails (missing API key)" $ do
      assertFail (client.v1.movies.latest identity {})
    test "Sub-route succeeds if parent route guard succeeds (has API key)" $ do
      assertRes (client.v1.movies.latest (withCookies (Map.singleton "apiKey" "key")) {})
                 { id: 723, title: "The Godfather" }
    test "Sub-route fails if passes parent guard but not child guard (missing session key)" $ do
      let payload = { movieId: 1, body: { value: 9.0 } }
      let opts = withCookies $ Map.singleton "apiKey" "key"
      assertFail $ client.v1.movies.byId.rating.create opts payload
    test "Sub-route succeeds if passes parent and child guards (has API and session keys)" $ do
      let opts = withCookies $ Map.fromFoldable [Tuple "apiKey" "key", Tuple "sessionId" "sessionId"]
      assertRes (client.v1.movies.byId.rating.create opts { movieId: 1, body: { value: 9.0 } })
                 { statusCode: 1, statusMessage: "Created" }

runTests :: Aff Unit
runTests = do
  let runTest = runTestWith Fancy.runTest (tests $ mkClient Client.defaultOpts moviesApiSpec)
  withServer moviesApiSpec moviesApi runTest
