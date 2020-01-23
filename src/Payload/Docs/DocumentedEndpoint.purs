module Payload.Docs.DocumentedEndpoint where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Payload.Client.Queryable (class EncodeOptionalQuery, class EncodeUrlWithParams)
import Payload.ContentType (class HasContentType, getContentType)
import Payload.Docs.JsonSchema (JsonSchema(JsonSchema), JsonSchemaType(..), jsonSchema)
import Payload.Docs.OpenApi (MediaTypeObject, OpenApiSpec, Operation, Param, ParamLocation(..), PathItem, Response, emptyOpenApi, emptyPathItem, mkOperation)
import Payload.Internal.Route (DefaultRouteSpec, Undefined(..))
import Payload.Spec (class IsSymbolList, Route, Tags(..), reflectSymbolList)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
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
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeOptionalQuery fullPath query payload

       , HasContentType res
       , ToJsonSchema res
       , ToJsonSchemaRowList fullParamsList
       , ToJsonSchemaQueryParams query
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
      urlParams = toUrlParam <$> toJsonSchemaRowList (RLProxy :: _ fullParamsList)

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

class ToJsonSchema a where
  toJsonSchema :: Proxy a -> JsonSchema

instance toJsonSchemaString :: ToJsonSchema String where
  toJsonSchema _ = jsonSchema (_ { "type" = Just JsonSchemaString })
instance toJsonSchemaNumber :: ToJsonSchema Number where
  toJsonSchema _ = jsonSchema (_ { "type" = Just JsonSchemaNumber })
instance toJsonSchemaInt :: ToJsonSchema Int where
  toJsonSchema _ = jsonSchema (_ { "type" = Just JsonSchemaInteger })
instance toJsonSchemaBoolean :: ToJsonSchema Boolean where
  toJsonSchema _ = jsonSchema (_ { "type" = Just JsonSchemaBoolean })
instance toJsonSchemaArray :: ToJsonSchema a => ToJsonSchema (Array a) where
  toJsonSchema _ = jsonSchema (_ { "type" = Just JsonSchemaArray
                                 , items = Just (toJsonSchema (Proxy :: _ a))})
instance toJsonSchemaList :: ToJsonSchema a => ToJsonSchema (List a) where
  toJsonSchema _ = jsonSchema (_ { "type" = Just JsonSchemaArray
                                 , items = Just (toJsonSchema (Proxy :: _ a))})
instance toJsonSchemaRecord :: ( ToJsonSchemaRowList rl
                               , RowToList a rl
                               ) => ToJsonSchema (Record a) where
  toJsonSchema _ = jsonSchema (_ { "type" = Just JsonSchemaObject
                                 , properties = Just properties
                                 , required = Just required})
    where
      fieldsJsonSchema :: Array FieldJsonSchema
      fieldsJsonSchema = toJsonSchemaRowList (RLProxy :: _ rl)

      properties :: Object JsonSchema
      properties = Object.fromFoldable $ fieldProp <$> fieldsJsonSchema

      fieldProp :: FieldJsonSchema -> Tuple String JsonSchema
      fieldProp {key, schema} = Tuple key schema

      required :: Array String
      required = fieldsJsonSchema
                 # Array.filter _.required
                 # map _.key

class ToJsonSchemaRowList (rl :: RowList) where
  toJsonSchemaRowList :: RLProxy rl -> Array FieldJsonSchema

instance toJsonSchemaRowListNil :: ToJsonSchemaRowList RowList.Nil where
  toJsonSchemaRowList _ = []

else instance toJsonSchemaRowListConsMaybe ::
  ( ToJsonSchema val
  , ToJsonSchemaRowList rest
  , IsSymbol key
  ) => ToJsonSchemaRowList (RowList.Cons key (Maybe val) rest) where
  toJsonSchemaRowList _ = Array.cons this rest
    where
      this :: FieldJsonSchema
      this = { key, required: false, schema }
      
      key :: String
      key = reflectSymbol (SProxy :: _ key)
      
      schema :: JsonSchema
      schema = toJsonSchema (Proxy :: _ val)
      
      rest :: Array FieldJsonSchema
      rest = toJsonSchemaRowList (RLProxy :: _ rest)

else instance toJsonSchemaRowListCons ::
  ( ToJsonSchema val
  , ToJsonSchemaRowList rest
  , IsSymbol key
  ) => ToJsonSchemaRowList (RowList.Cons key val rest) where
  toJsonSchemaRowList _ = Array.cons this rest
    where
      this :: FieldJsonSchema
      this = { key, required: true, schema }
      
      key :: String
      key = reflectSymbol (SProxy :: _ key)
      
      schema :: JsonSchema
      schema = toJsonSchema (Proxy :: _ val)
      
      rest :: Array FieldJsonSchema
      rest = toJsonSchemaRowList (RLProxy :: _ rest)

type FieldJsonSchema =
  { key :: String
  , required :: Boolean
  , schema :: JsonSchema }

class ToJsonSchemaQueryParams query where
  toJsonSchemaQueryParams :: Proxy query -> Array FieldJsonSchema

instance toJsonSchemaQueryParamsUndefined :: ToJsonSchemaQueryParams Undefined where
  toJsonSchemaQueryParams _ = []

instance toJsonSchemaQueryParamsRecord ::
  ( RowToList query queryList
  , ToJsonSchemaRowList queryList
  ) => ToJsonSchemaQueryParams (Record query) where
  toJsonSchemaQueryParams _ = toJsonSchemaRowList (RLProxy :: _ queryList)

methodPathItem :: String -> Operation -> PathItem
methodPathItem "GET" operation = emptyPathItem { get = Just operation }
methodPathItem "PUT" operation = emptyPathItem { put = Just operation }
methodPathItem "POST" operation = emptyPathItem { post = Just operation }
methodPathItem "DELETE" operation = emptyPathItem { delete = Just operation }
methodPathItem "OPTIONS" operation = emptyPathItem { options = Just operation }
methodPathItem "HEAD" operation = emptyPathItem { head = Just operation }
methodPathItem "PATCH" operation = emptyPathItem { patch = Just operation }
methodPathItem _ _ = emptyPathItem
