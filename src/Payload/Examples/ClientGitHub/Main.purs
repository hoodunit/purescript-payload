module Payload.Examples.ClientGitHub.Main where

import Prelude

import Data.DateTime (DateTime)
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (Foreign, ForeignError(..), fail)
import Payload.Client (defaultOpts, mkGuardedClient, unwrapBody)
import Payload.Client.Options (LogLevel(..))
import Payload.Debug (showDebug)
import Payload.Headers as Headers
import Payload.Spec (Spec(Spec), GET, Routes)
import Simple.JSON (class ReadForeign)

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
  , created_at :: Iso8601Date
  , updated_at :: Iso8601Date }

type User =
  { login :: String
  , id :: Number
  , node_id :: String
  , url :: String
  , "type" :: String }

newtype Iso8601Date = Iso8601Date DateTime

instance readForeignIso8601Date :: ReadForeign Iso8601Date where
  readImpl val = case JSDate.toDateTime (unsafeParseIso8601DateImpl val) of
    Nothing -> fail (ForeignError "Could not convert JSDate to DateTime")
    Just date -> pure (Iso8601Date date)

foreign import unsafeParseIso8601DateImpl :: Foreign -> JSDate

foreign import readEnvVarImpl :: Maybe String -> (String -> Maybe String) -> String -> Effect (Maybe String)

readEnvVar :: String -> Effect (Maybe String)
readEnvVar = readEnvVarImpl Nothing Just

main :: Effect Unit
main = do
  log "Running GitHub client example"
  tokenResult <- readEnvVar "GITHUB_AUTH_TOKEN"
  case tokenResult of
    Just _ -> log "Using GitHub token from GITHUB_AUTH_TOKEN"
    Nothing -> log "Running example without GitHub token"
  let authTokenHeaders = maybe [] (\token -> [Tuple "Authorization" ("token " <> token)]) tokenResult
  let extraHeaders = Headers.fromFoldable ([ Tuple "Accept" "application/vnd.github.v3+json" ] <> authTokenHeaders )
  let opts = defaultOpts { baseUrl = "https://api.github.com"
                         , logLevel = LogNormal
                         , extraHeaders = extraHeaders }
  let client = mkGuardedClient opts githubApiSpec
  launchAff_ $ do
    repos <- unwrapBody (client.repositories.list {query: {since: Nothing}})
    liftEffect $ log $ "Repos:\n" <> showDebug repos
    contributors <- unwrapBody (client.repos.byOwner.repo.contributors {params: {owner: "purescript", repo: "purescript"}})
    liftEffect $ log $ "Repo contributors:\n" <> showDebug contributors
    psRepo <- unwrapBody (client.repos.byOwner.repo.get {params: {owner: "purescript", repo: "purescript"}})
    liftEffect $ log $ "PureScript repo:\n" <> showDebug psRepo
    liftEffect $ log "Done running GitHub client example"
