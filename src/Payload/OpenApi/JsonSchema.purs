module Payload.OpenApi.JsonSchema where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Simple.JSON (class WriteForeign, writeImpl)

newtype JsonSchema = JsonSchema JsonSchemaData
type JsonSchemaData =
  { "type" :: Maybe JsonSchemaType
  , additionalProperties :: Maybe JsonSchema
  , description :: Maybe String
  , format :: Maybe String
  , items :: Maybe JsonSchema
  , oneOf :: Maybe (Array JsonSchema)
  , properties :: Maybe (Object JsonSchema)
  , ref :: Maybe JsonSchemaRef
  , required :: Maybe (Array String) }

emptyJsonSchema :: JsonSchemaData
emptyJsonSchema =
  { "type": Nothing
  , additionalProperties: Nothing
  , description: Nothing
  , format: Nothing
  , items: Nothing
  , oneOf: Nothing
  , properties: Nothing
  , ref: Nothing
  , required: Nothing }

jsonSchema :: (JsonSchemaData -> JsonSchemaData) -> JsonSchema
jsonSchema f = JsonSchema (f emptyJsonSchema)

instance showSchema :: Show JsonSchema where
  show (JsonSchema { "type": _type, additionalProperties, description, format, items, oneOf, properties, ref, required }) =
    "{\n  type: " <> show _type <>
    ",\n  additionalProperties: " <> show additionalProperties <>
    ",\n  description: " <> show description <>
    ",\n  format: " <> show format <>
    ",\n  items: " <> show items <>
    ",\n  oneOf: " <> show oneOf <>
    ",\n  properties: " <> show properties <>
    ",\n  ref: " <> show ref <>
    ",\n  required: " <> show required <> "}"
instance writeForeignJsonSchema :: WriteForeign JsonSchema where
  writeImpl (JsonSchema s) = writeImpl s

newtype JsonSchemaRef = JsonSchemaRef String
instance showJsonSchemaRef :: Show JsonSchemaRef where
  show (JsonSchemaRef ref) = show ref
instance writeForeignJsonSchemaRef :: WriteForeign JsonSchemaRef where
  writeImpl (JsonSchemaRef ref) = writeImpl ref
                        
data JsonSchemaType
  = JsonSchemaObject
  | JsonSchemaArray
  | JsonSchemaString
  | JsonSchemaNumber
  | JsonSchemaInteger
  | JsonSchemaBoolean
  | JsonSchemaNull

instance showJsonSchemaType :: Show JsonSchemaType where
  show JsonSchemaObject = "\"object\""
  show JsonSchemaArray = "\"array\""
  show JsonSchemaString = "\"string\""
  show JsonSchemaNumber = "\"number\""
  show JsonSchemaInteger = "\"integer\""
  show JsonSchemaBoolean = "\"boolean\""
  show JsonSchemaNull = "\"null\""
instance writeForeignJsonSchemaType :: WriteForeign JsonSchemaType where
  writeImpl JsonSchemaObject = writeImpl "object"
  writeImpl JsonSchemaArray = writeImpl "array"
  writeImpl JsonSchemaString = writeImpl "string"
  writeImpl JsonSchemaNumber = writeImpl "number"
  writeImpl JsonSchemaInteger = writeImpl "integer"
  writeImpl JsonSchemaBoolean = writeImpl "boolean"
  writeImpl JsonSchemaNull = writeImpl "null"
