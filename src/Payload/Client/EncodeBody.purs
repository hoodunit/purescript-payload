module Payload.Client.EncodeBody where

import Data.Maybe (Maybe(..))
import Payload.ContentType (class HasContentType)
import Simple.JSON as SimpleJson

class (HasContentType body) <= EncodeBody body where
  encodeBody :: body -> String

instance encodeBodyString :: EncodeBody String where
  encodeBody b = b

instance encodeBodyRecord :: SimpleJson.WriteForeign (Record r) => EncodeBody (Record r) where
  encodeBody = SimpleJson.writeJSON

instance encodeBodyArray :: SimpleJson.WriteForeign (Array r) => EncodeBody (Array r) where
  encodeBody = SimpleJson.writeJSON

instance encodeBodyMaybe :: EncodeBody a => EncodeBody (Maybe a) where
  encodeBody Nothing = ""
  encodeBody (Just body) = encodeBody body
