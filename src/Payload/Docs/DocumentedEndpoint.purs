module Payload.Docs.DocumentedEndpoint where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Object
import Payload.ContentType (class HasContentType, getContentType)
import Payload.Docs.JsonSchema (JsonSchema(JsonSchema))
import Payload.Docs.OpenApi (MediaTypeObject, OpenApiSpec, Operation, Param, ParamLocation(..), PathItem, Response, emptyOpenApi, emptyPathItem, mkOperation)
import Payload.Docs.ToJsonSchema (class ToJsonSchema, class ToJsonSchemaQueryParams, class ToJsonSchemaRowList, class ToJsonSchemaUrlParams, FieldJsonSchema, toJsonSchema, toJsonSchemaQueryParams, toJsonSchemaRowList, toJsonSchemaUrlParams)
import Payload.Internal.Route (DefaultRouteSpec)
import Payload.Spec (class IsSymbolList, Route, Tags(..), reflectSymbolList)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.Symbol as Symbol
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.RowList (RLProxy(..))

class DocumentedEndpoint
  route
  (basePath :: Symbol)
  (baseParams :: # Type)
  payload
  res
  | route baseParams basePath -> payload, route -> res where
  mkEndpointOpenApi :: route
             -> SProxy basePath
             -> Proxy (Record baseParams)
             -> OpenApiSpec

instance openApiEndpointRoute ::
       ( Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , query :: query
           , body :: body
           , summary :: SProxy summary
           , description :: SProxy description
           , tags :: Tags tags
           | r }
       , Row.Union baseParams params fullUrlParams
       , Symbol.Append basePath path fullPath
       , IsSymbol method
       , IsSymbol path
       , IsSymbol fullPath
       , IsSymbol summary
       , IsSymbol description
       , IsSymbol basePath
       , IsSymbolList tags
       , TypeEquals (Record payload)
           { body :: body
           | rest }

       , RowToList fullUrlParams fullParamsList

       , HasContentType res
       , ToJsonSchema res
       , ToJsonSchemaRowList fullParamsList
       , ToJsonSchemaQueryParams query
       , ToJsonSchemaUrlParams fullPath fullUrlParams
       )
    => DocumentedEndpoint (Route method path (Record route)) basePath baseParams (Record payload) res where
  mkEndpointOpenApi _ _ _ = emptyOpenApi { paths = paths }
    where
      paths :: Object PathItem
      paths = Object.singleton path pathItem

      path :: String
      path = toOpenApiPath (reflectSymbol (SProxy :: _ fullPath))

      summary :: Maybe String
      summary = case reflectSymbol (SProxy :: _ summary) of
        "" -> Nothing
        s -> Just s

      description :: Maybe String
      description = case reflectSymbol (SProxy :: _ description) of
        "" -> Nothing
        s -> Just s

      tags :: Maybe (Array String)
      tags = case reflectSymbolList (Tags :: _ tags) of
               List.Nil -> Nothing
               tagsList -> Just (List.toUnfoldable tagsList)

      pathItem :: PathItem
      pathItem = (methodPathItem (reflectSymbol (SProxy :: _ method)) operation)

      operation :: Operation
      operation = (mkOperation responses)
        { parameters = parameters
        , summary = summary <|> Just path
        , description = description <|> Just path
        , tags = fromMaybe [reflectSymbol (SProxy :: _ basePath)] tags }

      parameters :: Array Param
      parameters = urlParams <> queryParams

      urlParams :: Array Param
      urlParams = toUrlParam <$> toJsonSchemaUrlParams (SProxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      toUrlParam :: FieldJsonSchema -> Param
      toUrlParam {key, required, schema} =
        { name: key
        , "in": ParamInPath
        , description: Nothing
        , required
        , schema: Just schema }

      queryParams :: Array Param
      queryParams = toQueryParam <$> toJsonSchemaQueryParams (Proxy :: _ query)

      toQueryParam :: FieldJsonSchema -> Param
      toQueryParam {key, required, schema} =
        { name: key
        , "in": ParamInQuery
        , description: Nothing
        , required
        , schema: Just schema }

      responses :: Object Response
      responses = Object.singleton "200" response

      response :: Response
      response = { description: ""
                 , headers: Object.empty
                 , content: Object.singleton responseContent responseMedia }

      responseContent :: String
      responseContent = getContentType (Proxy :: _ res)

      responseMedia :: MediaTypeObject
      responseMedia = { schema: responseSchema }

      responseSchema :: JsonSchema
      responseSchema = toJsonSchema (Proxy :: _ res)

toOpenApiPath :: String -> String
toOpenApiPath str = str
  # String.replaceAll (Pattern "<") (Replacement "{")
  # String.replaceAll (Pattern ">") (Replacement "}")
  # String.split (Pattern "?")
  # Array.head
  # fromMaybe str

methodPathItem :: String -> Operation -> PathItem
methodPathItem "GET" operation = emptyPathItem { get = Just operation }
methodPathItem "PUT" operation = emptyPathItem { put = Just operation }
methodPathItem "POST" operation = emptyPathItem { post = Just operation }
methodPathItem "DELETE" operation = emptyPathItem { delete = Just operation }
methodPathItem "OPTIONS" operation = emptyPathItem { options = Just operation }
methodPathItem "HEAD" operation = emptyPathItem { head = Just operation }
methodPathItem "PATCH" operation = emptyPathItem { patch = Just operation }
methodPathItem _ _ = emptyPathItem
