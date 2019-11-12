module Payload.Examples.Movies.Main where

import Prelude

import Data.Either (Either, note)
import Data.Map as Map
import Effect.Aff (Aff)
import Node.HTTP as HTTP
import Payload.Cookies (requestCookies)
import Payload.Spec (type (:), Spec(Spec), DELETE, GET, Guards(..), POST, Route, Routes, Nil)

-- Example API based on The Movie Database API at
-- https://developers.themoviedb.org

moviesApiSpec :: Spec {
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
           delete :: DELETE "/" {
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
             delete :: DELETE "/rating" {
               response :: StatusCodeResponse
             }
           }
         }
      }
    }
  }
}
moviesApiSpec = Spec

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

moviesApi = {
  handlers: {
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
  },
  guards: {
    apiKey: getApiKey,
    sessionId: getSessionId
  }
}

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

getMovie :: forall r. { params :: { movieId :: Int } | r} -> Aff Movie
getMovie { params: { movieId } } = pure { id: movieId, title: "Fetched movie" }

createRating :: forall r.
                { params :: { movieId :: Int }
                , guards :: { apiKey :: ApiKey, sessionId :: SessionId}
                | r} -> Aff StatusCodeResponse
createRating _ = pure { statusCode: 1, statusMessage: "Created" }

deleteRating :: forall r.
                { params :: { movieId :: Int }
                , guards :: { apiKey :: ApiKey, sessionId :: SessionId }
                | r} -> Aff StatusCodeResponse
deleteRating _ = pure { statusCode: 1, statusMessage: "Deleted" }

getApiKey :: HTTP.Request -> Aff (Either String ApiKey)
getApiKey req = do
  let cookies = requestCookies req
  pure $ note "No cookie" $ Map.lookup "apiKey" cookies

getSessionId :: HTTP.Request -> Aff (Either String SessionId)
getSessionId req = do
  let cookies = requestCookies req
  pure $ note "No cookie" $ Map.lookup "sessionId" cookies
