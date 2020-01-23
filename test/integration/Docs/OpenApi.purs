module Payload.Test.Integration.Docs.OpenApi where

import Prelude

import Data.Foldable (all)
import Data.List (List)
import Data.Maybe (Maybe(..), isJust)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object
import Payload.Docs as Docs
import Payload.Docs.JsonSchema (JsonSchema(..), JsonSchemaType(..), emptyJsonSchema)
import Payload.Docs.OpenApi (ParamLocation(..))
import Payload.Docs.OpenApi as OpenApi
import Payload.Spec (GET, POST, PUT, Routes(..), Spec(Spec), DELETE)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
  
tests :: TestSuite
tests = do
  suite "OpenAPI" do
    suite "info" do
      test "title option appears in OpenAPI title" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/foo" { response :: String } } }
        let opts = Docs.defaultOpts { info { title = "My API" } }
        let openApiSpec = Docs.mkOpenApiSpec opts spec
        Assert.equal "My API" openApiSpec.info.title
      test "version option appears in OpenAPI version" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/foo" { response :: String } } }
        let opts = Docs.defaultOpts { info { version = "1.0" } }
        let openApiSpec = Docs.mkOpenApiSpec opts spec
        Assert.equal "1.0" openApiSpec.info.version
    suite "methods" do
      let assertMethodOperationExists openApiSpec op =
            Assert.equal' ("Expected OpenAPI spec to contain method: " <> Docs.toJson openApiSpec)
                          true
                          (isJust op)
      test "GET" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/foo" { response :: String } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let operation = openApiSpec.paths # Object.lookup "/foo" >>= _.get
        assertMethodOperationExists openApiSpec operation
      test "GET only defines OpenAPI 'get' operation" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/foo" { response :: String } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let getOp f = openApiSpec.paths # Object.lookup "/foo" >>= f
        let expectedOps =
              { put: Nothing
              , post: Nothing
              , delete: Nothing
              , options: Nothing
              , head: Nothing
              , patch: Nothing }
        let actualOps =
              { put: getOp _.put
              , post: getOp _.post
              , delete: getOp _.delete
              , options: getOp _.options
              , head: getOp _.head
              , patch: getOp _.patch }
        Assert.equal expectedOps actualOps
      test "POST" $ do
        let spec = Spec :: _ { routes :: { foo :: POST "/foo" { response :: String } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let operation = openApiSpec.paths # Object.lookup "/foo" >>= _.post
        assertMethodOperationExists openApiSpec operation
      test "PUT" $ do
        let spec = Spec :: _ { routes :: { foo :: PUT "/foo" { response :: String } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let operation = openApiSpec.paths # Object.lookup "/foo" >>= _.put
        assertMethodOperationExists openApiSpec operation
      test "DELETE" $ do
        let spec = Spec :: _ { routes :: { foo :: DELETE "/foo" { response :: String } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let operation = openApiSpec.paths # Object.lookup "/foo" >>= _.delete
        assertMethodOperationExists openApiSpec operation
    suite "URL params" do
      test "single required param" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/users/<userId>" {
                                             response :: String,
                                             params :: { userId :: String }
                                         } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let params = openApiSpec.paths # Object.lookup "/users/{userId}" >>= _.get <#> _.parameters
        let userParam =
              { name: "userId"
              , "in": ParamInPath
              , description: Nothing
              , required: true
              , schema: Just (JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaString })) }
        Assert.equal (Just [userParam]) params
      test "multi-match param is ignored (OpenAPI doesn't support it)" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/users/<..rest>" {
                                             response :: String,
                                             params :: { rest :: List String }
                                         } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let params = openApiSpec.paths # Object.lookup "/users/{..rest}" >>= _.get <#> _.parameters
        let itemSchema = Just (JsonSchema $ emptyJsonSchema { "type" = Just JsonSchemaString })
        let userParam =
              { name: "rest"
              , "in": ParamInPath
              , description: Nothing
              , required: true
              , schema: Just (JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaArray
                                                          , items = itemSchema })) }
        Assert.equal (Just []) params
