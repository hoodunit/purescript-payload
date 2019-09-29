module Payload.Examples.Movies.Main where

import Prelude

import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush, note)
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Payload.Client.Client (class ClientApi, mkClient)
import Payload.Client.Client as Client
import Payload.Cookies (requestCookies)
import Payload.Cookies as Cookies
import Payload.Guards (GuardFn)
import Payload.Handlers (File(..))
import Payload.Internal.GuardParsing (type (:), GuardTypes(..), Guards(..), Nil)
import Payload.Spec (API(API), GET, POST, Route, Routes, DELETE)
import Payload.Test.Helpers (assertFail, assertRes, withServer)
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- Example API based on The Movie Database API at
-- https://developers.themoviedb.org

type Movie =
  { id :: Int
  , title :: String }

type Date = String

type RequestTokenResponse =
  { success :: Boolean
  , expiresAt :: Date
  , requestToken :: String }

type SessionIdResponse =
  { success :: Boolean
  , sessionId :: Date
  , requestToken :: String }

type StatusResponse =
  { success :: Boolean }

type StatusCodeResponse =
  { statusCode :: Int
  , statusMessage :: String }

type RatingValue =
  { value :: Number }

type ApiKey = String
type SessionId = String
data Path (s :: Symbol) = Path

moviesApi :: API {
  guards :: {
     apiKey :: ApiKey,
     sessionId :: SessionId
  },
  routes :: {
    v1 :: Routes "/v1" {
       guards :: Guards ("apiKey" : Nil),
       auth :: Routes "/authentication" {
         token :: Routes "/token" {
           new :: GET "/new" {
             response :: RequestTokenResponse
           }
         },
         session :: Routes "/session" {
           create :: POST "/new" {
             body :: { requestToken :: String },
             response :: SessionIdResponse
           },
           -- DELETE not supported yet in client
           -- delete :: DELETE "/" {
           delete :: GET "/" {
             body :: { sessionId :: String },
             response :: StatusResponse
           }
         }
       },
       movies :: Routes "/movies" {
         latest :: GET "/latest" {
           response :: Movie
         },
         popular :: GET "/popular" {
           response :: { results :: Array Movie }
         },
         byId :: Routes "/<movieId>" {
           params :: { movieId :: Int },
           get :: GET "/" {
             response :: Movie
           },
           rating :: Routes "/rating" {
             guards :: Guards ("sessionId" : Nil),
             create :: POST "/rating" {
               body :: RatingValue,
               response :: StatusCodeResponse
             },
             -- DELETE not supported yet in client
             -- delete :: DELETE "/rating" {
             delete :: GET "/rating" {
               response :: StatusCodeResponse
             }
           }
         }
      }
    }
  }
}
moviesApi = API

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

newToken :: forall r. { | r} -> Aff RequestTokenResponse
newToken _ = pure { success: true, expiresAt: "date", requestToken: "328dsdweoi" }

createSession :: forall r. { | r} -> Aff SessionIdResponse
createSession _ = pure { success: true, sessionId: "date", requestToken: "23988w9" }

deleteSession :: forall r. { | r} -> Aff StatusResponse
deleteSession _ = pure { success: true }

latestMovie :: forall r. { | r} -> Aff Movie
latestMovie _ = pure $ { id: 723, title: "The Godfather" }

popularMovies :: forall r. { | r} -> Aff { results :: Array Movie }
popularMovies _ = pure { results: [
  { id: 723, title: "The Godfather" },
  { id: 722, title: "Citizen Kane" }] }

getMovie :: forall r. { movieId :: Int | r} -> Aff Movie
getMovie { movieId } = pure { id: movieId, title: "Fetched movie" }

createRating :: forall r.
                { movieId :: Int, apiKey :: ApiKey, sessionId :: SessionId | r}
                -> Aff StatusCodeResponse
createRating _ = pure { statusCode: 1, statusMessage: "Created" }

deleteRating :: forall r.
                { movieId :: Int, apiKey :: ApiKey, sessionId :: SessionId | r}
                -> Aff StatusCodeResponse
deleteRating _ = pure { statusCode: 1, statusMessage: "Deleted" }

handlers = {
  v1: {
    auth: {
      token: {
        new: newToken
      },
      session: {
        create: createSession,
        delete: deleteSession
      }
    },
    movies: {
      latest: latestMovie,
      popular: popularMovies,
      byId: {
        get: getMovie,
        rating: {
          create: createRating,
          delete: deleteRating
        }
      }
    }
  }
}

getApiKey :: GuardFn ApiKey
getApiKey req = do
  let cookies = requestCookies req
  pure $ note "No cookie" $ Map.lookup "apiKey" cookies

getSessionId :: GuardFn SessionId
getSessionId req = do
  let cookies = requestCookies req
  pure $ note "No cookie" $ Map.lookup "sessionId" cookies

guards = {
  apiKey: getApiKey,
  sessionId: getSessionId
}

runTests :: Aff Unit
runTests = do
  let runTest = runTestWith Fancy.runTest (tests $ mkClient Client.defaultOpts moviesApi)
  withServer moviesApi { handlers, guards } runTest
