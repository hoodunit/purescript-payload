module Payload.Client.DecodeResponse where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Node.Stream as Stream
import Payload.Internal.TypeErrors (type (<>), type (|>))
import Payload.Response (ResponseBody(..))
import Prim.TypeError (class Warn, Quote, Text)
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

class DecodeResponse r where
  decodeResponse :: ResponseBody -> Either String r

instance decodeResponseString :: DecodeResponse String where
  decodeResponse (StringBody s) = Right s
  decodeResponse _ = Left "Invalid response type, expected String"
else instance decodeResponseStream ::
  ( TypeEquals (Stream.Stream r) (Stream.Stream (read :: Stream.Read | r'))
  ) => DecodeResponse (Stream.Stream r) where
  decodeResponse (StreamBody s) = Right (unsafeCoerce s)
  decodeResponse _ = Left "Invalid response type, expected stream"
else instance decodeResponseRecord ::
  ( SimpleJson.ReadForeign (Record r)
  ) => DecodeResponse (Record r) where
  decodeResponse (StringBody s) = lmap show $ SimpleJson.readJSON s
  decodeResponse _ = Left "Invalid response type, expected String"
else instance decodeResponseArray ::
  ( SimpleJson.ReadForeign (Array r)
  ) => DecodeResponse (Array r) where
  decodeResponse (StringBody s) = lmap show $ SimpleJson.readJSON s
  decodeResponse _ = Left "Invalid response type, expected String"
-- | Adding a default instance allows the client to be incomplete:
-- | not all responses are supported.
else instance decodeResponseDefault ::
  Warn (Text "API client cannot query all of endpoints in API spec:"
          |> Text ""
          |> Text "No type class instance was found for"
          |> Text ""
          |> Text "DecodeResponse " <> Quote a
          |> Text "")
  => DecodeResponse a where
  decodeResponse _ = Left "Could not decode response - no DecodeResponse instance"
