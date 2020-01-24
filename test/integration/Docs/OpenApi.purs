module Payload.Test.Integration.Docs.OpenApi where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Payload.Docs as Docs
import Payload.Docs.JsonSchema (JsonSchema(..), JsonSchemaType(..), emptyJsonSchema)
import Payload.Docs.OpenApi (ParamLocation(..))
import Payload.Spec (GET, POST, PUT, Spec(Spec), DELETE)
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
      test "multi-match param is ignored in URL params as OpenAPI doesn't support it (but it shows in URL)" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/users/<..rest>" {
                                             response :: String,
                                             params :: { rest :: List String }
                                         } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let params = openApiSpec.paths # Object.lookup "/users/{..rest}" >>= _.get <#> _.parameters
        Assert.equal (Just []) params
    suite "Query params" do
      test "required key param" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/search?q=<q>" {
                                             response :: String,
                                             query :: { q :: String }
                                         } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let params = openApiSpec.paths # Object.lookup "/search" >>= _.get <#> _.parameters
        let param =
              { name: "q"
              , "in": ParamInQuery
              , description: Nothing
              , required: true
              , schema: Just (JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaString })) }
        Assert.equal (Just [param]) params
      test "optional key param" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/search?q=<q>" {
                                             response :: String,
                                             query :: { q :: Maybe String }
                                         } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let params = openApiSpec.paths # Object.lookup "/search" >>= _.get <#> _.parameters
        let param =
              { name: "q"
              , "in": ParamInQuery
              , description: Nothing
              , required: false
              , schema: Just (JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaString })) }
        Assert.equal (Just [param]) params
      test "multi-match" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/search?<..all>" {
                                             response :: String,
                                             query :: { all :: Object (Array String) }
                                         } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let params = openApiSpec.paths # Object.lookup "/search" >>= _.get <#> _.parameters
        let strSchema = Just (JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaString }))
        let arrayStrSchema = Just (JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaArray
                                                               , items = strSchema }))
        let objectSchema = Just (JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaObject
                                                             , additionalProperties = arrayStrSchema }))
        let param =
              { name: "all"
              , "in": ParamInQuery
              , description: Nothing
              , required: true
              , schema: objectSchema }
        Assert.equal (Just [param]) params
    suite "Response bodies" do
      test "simple string response has correct status and encoding" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/search" {response :: String} } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let responses = openApiSpec.paths
                     # Object.lookup "/search"
                     >>= _.get
                     <#> _.responses
        let headers = Object.empty
        let schema = JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaString })
        let content = Object.fromFoldable
                        [ Tuple "text/plain; charset=utf-8" { schema } ]
        let expectedResponses = Object.fromFoldable
                                  [ Tuple "2XX" { description: "", headers, content } ]
        Assert.equal (Just expectedResponses) responses
      test "simple string response has correct status and encoding" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/search" {response :: String} } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let responses = openApiSpec.paths
                     # Object.lookup "/search"
                     >>= _.get
                     <#> _.responses
        let headers = Object.empty
        let schema = JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaString })
        let content = Object.fromFoldable
                        [ Tuple "text/plain; charset=utf-8" { schema } ]
        let expectedResponses = Object.fromFoldable
                                  [ Tuple "2XX" { description: "", headers, content } ]
        Assert.equal (Just expectedResponses) responses
      test "Record response has correct status and encoding" $ do
        let spec = Spec :: _ { routes :: { foo :: GET "/search" {response :: { foo :: Int } } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let responses = openApiSpec.paths
                     # Object.lookup "/search"
                     >>= _.get
                     <#> _.responses
        let headers = Object.empty
        let intSchema = JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaInteger })
        let props = Object.fromFoldable [Tuple "foo" intSchema]
        let schema = JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaObject
                                                 , properties = Just props
                                                 , required = Just ["foo"] })
        let content = Object.fromFoldable
                        [ Tuple "application/json" { schema } ]
        let expectedResponses = Object.fromFoldable
                                  [ Tuple "2XX" { description: "", headers, content } ]
        Assert.equal (Just expectedResponses) responses
    suite "Request bodies" do
      test "simple string body has correct encoding" $ do
        let spec = Spec :: _ { routes :: { foo :: POST "/search" {
                                                body :: String,
                                                response :: String
                                              } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let body = openApiSpec.paths
                   # Object.lookup "/search"
                   >>= _.post
                   >>= _.requestBody
        let schema = JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaString })
        let content = Object.fromFoldable
                        [ Tuple "text/plain; charset=utf-8" { schema } ]
        let expectedBody = { description: "", content, required: true }
        Assert.equal (Just expectedBody) body
      test "Record body has correct encoding (JSON)" $ do
        let spec = Spec :: _ { routes :: { foo :: POST "/search" {
                                                body :: { foo :: Int },
                                                response :: String
                                              } } }
        let openApiSpec = Docs.mkOpenApiSpec_ spec
        let body = openApiSpec.paths
                   # Object.lookup "/search"
                   >>= _.post
                   >>= _.requestBody
        let intSchema = JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaInteger })
        let props = Object.fromFoldable [Tuple "foo" intSchema]
        let schema = JsonSchema (emptyJsonSchema { "type" = Just JsonSchemaObject
                                                 , properties = Just props
                                                 , required = Just ["foo"] })
        let content = Object.fromFoldable
                        [ Tuple "application/json" { schema } ]
        let expectedBody = { description: "", content, required: true }
        Assert.equal (Just expectedBody) body
