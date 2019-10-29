module Payload.Client.FromResponse where

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

class FromResponse r where
  fromResponse :: ResponseBody -> Either String r

instance fromResponseString :: FromResponse String where
  fromResponse (StringBody s) = Right s
  fromResponse _ = Left "Invalid response type, expected String"
else instance fromResponseStream ::
  ( TypeEquals (Stream.Stream r) (Stream.Stream (read :: Stream.Read | r'))
  ) => FromResponse (Stream.Stream r) where
  fromResponse (StreamBody s) = Right (unsafeCoerce s)
  fromResponse _ = Left "Invalid response type, expected stream"
else instance fromResponseRecord ::
  ( SimpleJson.ReadForeign (Record r)
  ) => FromResponse (Record r) where
  fromResponse (StringBody s) = lmap show $ SimpleJson.readJSON s
  fromResponse _ = Left "Invalid response type, expected String"
else instance fromResponseArray ::
  ( SimpleJson.ReadForeign (Array r)
  ) => FromResponse (Array r) where
  fromResponse (StringBody s) = lmap show $ SimpleJson.readJSON s
  fromResponse _ = Left "Invalid response type, expected String"
-- | Adding a default instance allows the client to be incomplete:
-- | not all responses are supported.
else instance fromResponseDefault ::
  Warn (Text "API client cannot query all of endpoints in API spec:"
          |> Text ""
          |> Text "No type class instance was found for"
          |> Text ""
          |> Text "FromResponse " <> Quote a
          |> Text "")
  => FromResponse a where
  fromResponse _ = Left "Could not decode response - no FromResponse instance"
