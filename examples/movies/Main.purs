module Payload.Examples.Movies.Main where

import Prelude

import Data.Either (Either, note)
import Data.List (List)
import Data.Map as Map
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Node.HTTP as HTTP
import Payload.ContentType as ContentType
import Payload.Docs as Docs
import Payload.Docs.OpenApi (OpenApiSpec)
import Payload.Headers as Headers
import Payload.ResponseTypes (Response(..))
import Payload.Server as Payload
import Payload.Server.Cookies (requestCookies)
import Payload.Server.Handlers (File(..))
import Payload.Server.Response as Response
import Payload.Spec (type (:), DELETE, GET, Guards(..), Nil, POST, Route, Routes, Spec(Spec), Tags(..))

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
             summary :: SProxy "Create Request Token",
             description :: SProxy "Create a temporary request token that can be used to validate a TMDb user login. \
                                   \More details about how this works can be found here.",
             tags :: Tags ("Authentication" : Nil),
             response :: RequestTokenResponse
           }
         },
         session :: Routes "/session" {
           create :: POST "/new" {
             summary :: SProxy "Create Session",
             description :: SProxy "You can use this method to create a fully valid session ID once a user has \
                                   \validated the request token. More information about how this works can be found here.",
             tags :: Tags ("Session" : Nil),
             body :: { requestToken :: String },
             response :: SessionIdResponse
           },
           delete :: DELETE "/" {
             summary :: SProxy "Delete Session",
             description :: SProxy "If you would like to delete (or \"logout\") from a session, call this method with a valid session ID.",
             tags :: Tags ("Session" : Nil),
             body :: { sessionId :: String },
             response :: StatusResponse
           }
         }
       },
       movies :: Routes "/movies" {
         latest :: GET "/latest" {
           summary :: SProxy "Get Latest",
           description :: SProxy "Get the most newly created movie. This is a live response and will continuously change.",
           tags :: Tags ("Movies" : Nil),
           response :: Movie
         },
         popular :: GET "/popular/<..rest>" {
           summary :: SProxy "Get Popular",
           description :: SProxy "Get a list of the current popular movies on TMDb. This list updates daily.",
           tags :: Tags ("Movies" : Nil),
           params :: { rest :: List String },
           response :: { results :: Array Movie }
         },
         byId :: Routes "/<movieId>" {
           params :: { movieId :: Int },
           get :: GET "/" {
             summary :: SProxy "Get Details",
             description :: SProxy "Get the primary information about a movie.\n\n\
                                   \Supports append_to_response. Read more about this here.",
             tags :: Tags ("Movies" : Nil),
             response :: Movie
           },
           rating :: Routes "/rating" {
             guards :: Guards ("sessionId" : Nil),
             create :: POST "/rating" {
               summary :: SProxy "Rate Movie",
               description :: SProxy "Rate a movie.\n\n\
                                     \A valid session or guest session ID is required. You can read more about how this works here.",
               tags :: Tags ("Movies" : Nil),
               body :: RatingValue,
               response :: StatusCodeResponse
             },
             delete :: DELETE "/rating" {
               summary :: SProxy "Delete Rating",
               description :: SProxy "Remove your rating for a movie.\n\n\
                                     \A valid session or guest session ID is required. You can read more about how this works here.",
               tags :: Tags ("Movies" : Nil),
               response :: StatusCodeResponse
             }
           }
         }
      }
    },
    docs :: Docs.DocsEndpoint "/docs"
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

newToken :: forall r. { | r} -> Aff RequestTokenResponse
newToken _ = pure { success: true, expiresAt: "date", requestToken: "328dsdweoi" }

createSession :: forall r. { | r} -> Aff SessionIdResponse
createSession _ = pure { success: true, sessionId: "date", requestToken: "23988w9" }

deleteSession :: { body :: { sessionId :: String }
                 , guards :: { apiKey :: String }
                 } -> Aff StatusResponse
deleteSession _ = pure { success: true }

latestMovie :: forall r. { | r} -> Aff Movie
latestMovie _ = pure $ { id: 723, title: "The Godfather" }

popularMovies :: forall r. { | r} -> Aff { results :: Array Movie }
popularMovies _ = pure { results: [
  { id: 723, title: "The Godfather" },
  { id: 722, title: "Citizen Kane" }] }

getMovie :: forall r. { params :: { movieId :: Int } | r} -> Aff Movie
getMovie { params: { movieId } } = pure { id: movieId, title: "Fetched movie" }

createRating :: { params :: { movieId :: Int }
                , guards :: { apiKey :: ApiKey, sessionId :: SessionId}
                , body :: RatingValue
                } -> Aff StatusCodeResponse
createRating _ = pure { statusCode: 1, statusMessage: "Created" }

deleteRating :: { params :: { movieId :: Int }
                , guards :: { apiKey :: ApiKey, sessionId :: SessionId }
                } -> Aff StatusCodeResponse
deleteRating _ = pure { statusCode: 1, statusMessage: "Deleted" }

getApiKey :: HTTP.Request -> Aff (Either String ApiKey)
getApiKey req = do
  let cookies = requestCookies req
  pure $ note "No cookie" $ Map.lookup "apiKey" cookies

getSessionId :: HTTP.Request -> Aff (Either String SessionId)
getSessionId req = do
  let cookies = requestCookies req
  pure $ note "No cookie" $ Map.lookup "sessionId" cookies

main :: Effect Unit
main = Aff.launchAff_ $ do
  let serverInfo = { title: "The Movie Database API", version: "0.1.0" }
  let docsOpts = Docs.defaultOpts { info = serverInfo }
  let moviesApi = {
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
      },
      docs: Docs.docsHandler docsOpts moviesApiSpec
    },
    guards: {
      apiKey: getApiKey,
      sessionId: getSessionId
    }
  }
  Payload.startGuarded (Payload.defaultOpts { port = 3002 }) moviesApiSpec moviesApi
