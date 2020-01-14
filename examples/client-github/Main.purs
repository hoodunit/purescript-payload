module Payload.Examples.ClientGitHub.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Client (ClientResponse, defaultOpts, mkGuardedClient, unwrapBody)
import Payload.Client.Options (LogLevel(..))
import Payload.Debug (showDebug)
import Payload.Spec (type (:), Spec(Spec), DELETE, GET, Guards(..), POST, Route, Routes, Nil)

githubApiSpec :: Spec {
  guards :: {
  },
  routes :: {
    repositories :: Routes "/repositories" {
      list :: GET "/?since=<since>" {
         query :: { since :: Maybe Int },
         response :: Array Repository
      }
    },
    repos :: Routes "/repos" {
      byOwner :: Routes "/<owner>" {
         params :: {
           owner :: String
         },
         repo :: Routes "/<repo>" {
           params :: {
              repo :: String
           },
           get :: GET "/" {
             response :: FullRepository
           },
           contributors :: GET "/contributors" {
             response :: Array User
           }
         }
      }
    }
  }
}
githubApiSpec = Spec

type Repository =
  { id :: Int
  , node_id :: String
  , name :: String
  , full_name :: String
  , owner :: User
  , private :: Boolean
  , html_url :: String
  , description :: Maybe String
  , fork :: Boolean }

type FullRepository =
  { id :: Int
  , node_id :: String
  , name :: String
  , full_name :: String
  , owner :: User
  , private :: Boolean
  , html_url :: String
  , description :: Maybe String
  , fork :: Boolean
  , pushed_at :: String
  , created_at :: String
  , updated_at :: String }

type User =
  { login :: String
  , id :: Number
  , node_id :: String
  , url :: String
  , "type" :: String }

main :: Effect Unit
main = do
  log "Running GitHub client example"
  let opts = defaultOpts { baseUrl = "https://api.github.com", logLevel = LogNormal }
  let client = mkGuardedClient opts githubApiSpec
  launchAff_ $ do
    repos <- unwrapBody (client.repositories.list {query: {since: Nothing}})
    liftEffect $ log $ "Repos:\n" <> showDebug repos
    contributors <- unwrapBody (client.repos.byOwner.repo.contributors {params: {owner: "purescript", repo: "purescript"}})
    liftEffect $ log $ "Repo contributors:\n" <> showDebug contributors
    psRepo <- unwrapBody (client.repos.byOwner.repo.get {params: {owner: "purescript", repo: "purescript"}})
    liftEffect $ log $ "PureScript repo:\n" <> showDebug psRepo
    liftEffect $ log "Done running GitHub client example"
