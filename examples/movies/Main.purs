module Payload.Examples.Movies.Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.String as String
import Effect.Aff (Aff)
import Payload.Client (class ClientApi, mkClient)
import Payload.Client as Client
import Payload.GuardParsing (type (:), GuardTypes(..), Guards(..), Nil)
import Payload.Guards (GuardFn)
import Payload.Handlers (File(..))
import Payload.Route (GET, POST, Route(..), DELETE)
import Payload.Routing (API(..), Routes(..))
import Payload.Test.Helpers (withServer)
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
           -- DELETE not supported yet
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
               response :: StatusCodeResponse
             },
             -- DELETE not supported yet
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

assertResp :: forall a err. Show err => Eq a => Show a => Aff (Either err a) -> a -> Test
assertResp req expected = do
  res <- req
  case res of
    Right val -> Assert.equal expected val
    Left errors -> failure $ "Request failed: " <> show errors
  
-- tests :: forall routesSpec client r
--          . ClientApi routesSpec client
--          => API { routes :: routesSpec | r } -> TestSuite
tests client = do
  suite "Example: movies API" do
    test "Sub-route fails if parent route guard fails" $ do
      res <- client.v1.movies.latest Client.defaultOpts {}
      assertResp (client.v1.movies.latest Client.defaultOpts {})
                 { id: 999, title: "The Godfather" }
    test "Sub-route succeeds if parent route guard succeeds" $ do
      assertResp (client.v1.movies.latest Client.defaultOpts {})
                 { id: 723, title: "The Godfather" }

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
getApiKey req = pure $ unsafeCoerce {}

getSessionId :: GuardFn SessionId
getSessionId req = pure $ unsafeCoerce {}

guards = {
  apiKey: getApiKey,
  sessionId: getSessionId
}

runTests :: Aff Unit
runTests = do
  let runTest = runTestWith Fancy.runTest (tests $ mkClient moviesApi)
  withServer moviesApi { handlers, guards } runTest
