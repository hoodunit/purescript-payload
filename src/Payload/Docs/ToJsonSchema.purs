module Payload.Docs.ToJsonSchema where

import Prelude

import Data.Array as Array
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Payload.ContentType (class HasContentType, getContentType)
import Payload.Docs.JsonSchema (JsonSchema(JsonSchema), JsonSchemaType(..), jsonSchema)
import Payload.Docs.OpenApi as OpenApi
import Payload.Internal.Route (Undefined(..))
import Payload.Internal.UrlParsing (class ParseUrl, UrlCons, UrlListProxy(..), UrlNil, kind UrlList, Lit, Multi, Key)
import Payload.TypeErrors (type (<>), type (|>))
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
import Prim.TypeError (class Warn, Text)
import Type.Proxy (Proxy(..))
import Type.RowList (RLProxy(..))

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
instance toJsonSchemaObject :: ToJsonSchema a => ToJsonSchema (Object a) where
  toJsonSchema _ = jsonSchema (_ { "type" = Just JsonSchemaObject
                                 , additionalProperties = Just (toJsonSchema (Proxy :: _ a)) })
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


class ToJsonSchemaUrlParams (url :: Symbol) (params :: # Type) where
  toJsonSchemaUrlParams :: SProxy url -> Proxy (Record params) -> Array FieldJsonSchema

instance toJsonSchemaUrlParamsRecord ::
  ( ParseUrl url urlParts
  , ToJsonSchemaUrlParamsList urlParts params
  ) => ToJsonSchemaUrlParams url params where
  toJsonSchemaUrlParams _ _ = toJsonSchemaUrlParamsList (UrlListProxy :: _ urlParts) (Proxy :: _ (Record params))


class ToJsonSchemaUrlParamsList (urlParts :: UrlList) (params :: # Type) where
  toJsonSchemaUrlParamsList :: UrlListProxy urlParts -> Proxy (Record params) -> Array FieldJsonSchema

instance toJsonSchemaUrlParamsListNil :: ToJsonSchemaUrlParamsList UrlNil params where
  toJsonSchemaUrlParamsList _ _ = []

instance toJsonSchemaUrlParamsListConsLit ::
  ToJsonSchemaUrlParamsList rest params
  => ToJsonSchemaUrlParamsList (UrlCons (Lit lit) rest) params where
  toJsonSchemaUrlParamsList _ _ = toJsonSchemaUrlParamsList (UrlListProxy :: _ rest) (Proxy :: _ (Record params))

instance toJsonSchemaUrlParamsListConsMulti ::
  Warn (Text "URL multi-match parameter '<.." <> Text multi <> Text "> cannot be encoded in OpenAPI specification for documentation"
             |> Text "because OpenAPI does not support wildcard URL parameters."
             |> Text "The parameter will not appear in documentation.")
  => ToJsonSchemaUrlParamsList (UrlCons (Multi multi) rest) params where
  toJsonSchemaUrlParamsList _ _ = []

instance toJsonSchemaUrlParamsListConsKey ::
  ( IsSymbol key
  , Row.Cons key fieldVal otherParams params
  , ToJsonSchemaUrlParamsList rest otherParams
  , ToJsonSchema fieldVal
  ) => ToJsonSchemaUrlParamsList (UrlCons (Key key) rest) params where
  toJsonSchemaUrlParamsList _ _ = [
    { key: reflectSymbol (SProxy :: _ key)
    , required: true
    , schema: toJsonSchema (Proxy :: _ fieldVal) } ]

class ToJsonSchemaBody body where
  toJsonSchemaBody :: Proxy body -> Maybe OpenApi.RequestBody

instance toJsonSchemaBodyUndefined :: ToJsonSchemaBody Undefined where
  toJsonSchemaBody _ = Nothing

else instance toJsonSchemaBodyDefined ::
  ( ToJsonSchema body
  , HasContentType body
  ) => ToJsonSchemaBody body where
  toJsonSchemaBody _ = Just {content, description, required}
    where
      contentType = getContentType (Proxy :: _ body)
      mediaTypeObject = { schema: (toJsonSchema (Proxy :: _ body)) }
      content = Object.fromFoldable [ Tuple contentType mediaTypeObject ]
      description = ""
      required = true
