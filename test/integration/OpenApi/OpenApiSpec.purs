module Payload.Test.Integration.OpenApi.OpenApiSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Debug (formatJsonString, showDebug)
import Payload.OpenApi (mkOpenApiSpec, toJson)
import Payload.OpenApi as OpenApiSpec
import Payload.Spec (DELETE, GET, HEAD, POST, PUT, Routes(..), Spec(Spec))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

apiSpec :: Spec {
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
apiSpec = Spec

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
  
tests :: TestSuite
tests = do
  suite "OpenAPI" do
    test "can generate OpenAPI spec" $ do
      let opts = OpenApiSpec.defaultOpts { baseUrl = Just "https://api.github.com" }
      let openApiSpec = mkOpenApiSpec opts apiSpec
      liftEffect (log $ "\n\n" <> formatJsonString (toJson openApiSpec) <> "\n")
      Assert.equal "fail" (toJson openApiSpec)
