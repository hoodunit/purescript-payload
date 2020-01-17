module Payload.OpenApi.OpenApiTypes where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Payload.OpenApi.JsonSchema (JsonSchema(JsonSchema))
import Simple.JSON (class WriteForeign, writeImpl)

type OpenApi =
  { openapi :: String
  , info :: Info
  , paths :: Object PathItem
  , servers :: Array Server
  }

type Info =
  { title :: String
  , version :: String }

type PathItem =
  { summary :: Maybe String
  , description :: Maybe String
  , get :: Maybe Operation
  , put :: Maybe Operation
  , post :: Maybe Operation
  , delete :: Maybe Operation
  , options :: Maybe Operation
  , head :: Maybe Operation
  , patch :: Maybe Operation }

type Operation =
  { summary :: Maybe String
  , operationId :: Maybe String
  , parameters :: Array Param
  , requestBody :: Maybe RequestBody
  , responses :: Object Response }

type RequestBody = 
  { description :: String
  , content :: Object MediaTypeObject
  , required :: Boolean }

type MediaTypeObject =
  { schema :: JsonSchema }
    
type Response = 
  { description :: String
  , headers :: Object String
  , content :: Object MediaTypeObject }
    
type OpenApiSchema =
  { "$ref" :: Maybe String
  , "type" :: Maybe String }

type Param = 
  { name :: String
  , "in" :: ParamLocation
  , description :: Maybe String
  , required :: Boolean
  , schema :: Maybe JsonSchema }

data ParamLocation = ParamInQuery | ParamInHeader | ParamInPath | ParamInCookie

instance showParamLocation :: Show ParamLocation where
  show ParamInQuery = "ParamInQuery"
  show ParamInHeader = "ParamInHeader"
  show ParamInPath = "ParamInPath"
  show ParamInCookie = "ParamInCookie"
instance writeForeignParamLocation :: WriteForeign ParamLocation where
  writeImpl ParamInQuery = writeImpl "query"
  writeImpl ParamInHeader = writeImpl "header"
  writeImpl ParamInPath = writeImpl "path"
  writeImpl ParamInCookie = writeImpl "cookie"

type Server =
  { url :: String
  , description :: Maybe String }

emptyOpenApi :: OpenApi
emptyOpenApi =
  { openapi: "3.0.2"
  , info: { title: "Payload OpenAPI", version: "0.0.0" }
  , paths: Object.empty
  , servers: []
  }

emptyPathItem :: PathItem
emptyPathItem =
  { summary: Nothing
  , description: Nothing
  , get: Nothing
  , put: Nothing
  , post: Nothing
  , delete: Nothing
  , options: Nothing
  , head: Nothing
  , patch: Nothing }

mkOperation :: Object Response -> Operation
mkOperation responses =
  { summary: Nothing
  , operationId: Nothing
  , parameters: []
  , requestBody: Nothing
  , responses }

union :: OpenApi -> OpenApi -> OpenApi
union api1 api2 = { paths: Object.union api2.paths api1.paths
                  , openapi: api2.openapi
                  , info: api2.info
                  , servers: api1.servers <> api2.servers
                  }
