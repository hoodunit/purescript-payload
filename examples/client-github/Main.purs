module Payload.Examples.ClientGitHub.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Client (ClientResponse, defaultOpts, mkGuardedClient)
import Payload.Client.Options (LogLevel(..))
import Payload.Debug (showDebug)
import Payload.Spec (type (:), Spec(Spec), DELETE, GET, Guards(..), POST, Route, Routes, Nil)

githubApiSpec :: Spec {
  guards :: {
  },
  routes :: {
    repos :: Routes "/repositories" {
      list :: GET "/?since=<since>" {
         query :: { since :: Maybe Int },
         response :: Array Repository
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
  , owner :: Owner
  , private :: Boolean
  , html_url :: String
  , description :: Maybe String
  , fork :: Boolean }

type Owner =
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
    (repos :: ClientResponse (Array Repository)) <- client.repos.list {query: {since: Nothing}}
    liftEffect $ log (showDebug repos)
    liftEffect $ log "Done running GitHub client example"
