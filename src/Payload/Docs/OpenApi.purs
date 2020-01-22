module Payload.Docs.OpenApi where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Payload.Docs.JsonSchema (JsonSchema)
import Simple.JSON (class WriteForeign, writeImpl)

type OpenApiSpec =
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
  , description :: Maybe String
  , tags :: Array String
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

emptyOpenApi :: OpenApiSpec
emptyOpenApi =
  { openapi: "3.0.2"
  , info: defaultInfo
  , paths: Object.empty
  , servers: []
  }

defaultInfo :: Info
defaultInfo = { title: "Payload Live API Documentation", version: "0.0.0" }

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
  , description: Nothing
  , tags: []
  , operationId: Nothing
  , parameters: []
  , requestBody: Nothing
  , responses }

union :: OpenApiSpec -> OpenApiSpec -> OpenApiSpec
union api1 api2 = { paths: Object.union api2.paths api1.paths
                  , openapi: api2.openapi
                  , info: api2.info
                  , servers: api1.servers <> api2.servers
                  }
