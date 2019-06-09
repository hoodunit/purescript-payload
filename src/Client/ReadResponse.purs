module Payload.Client.FromResponse where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Node.Stream as Stream
import Payload.Response (ResponseBody(..))
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

class ReadResponse r where
  readResponse :: forall s. ResponseBody s -> Either String r

instance readResponseString :: ReadResponse String where
  readResponse (StringBody s) = Right s
  readResponse _ = Left "Invalid response type, expected String"
else instance readResponseStream ::
  ( TypeEquals (Stream.Stream r) (Stream.Stream (read :: Stream.Read | r'))
  ) => ReadResponse (Stream.Stream r) where
  readResponse (StreamBody s) = Right (unsafeCoerce s)
  readResponse _ = Left "Invalid response type, expected stream"
else instance readResponseRecord ::
  ( SimpleJson.ReadForeign (Record r)
  ) => ReadResponse (Record r) where
  readResponse (StringBody s) = lmap show $ SimpleJson.readJSON s
  readResponse _ = Left "Invalid response type, expected String"
else instance readResponseArray ::
  ( SimpleJson.ReadForeign (Array r)
  ) => ReadResponse (Array r) where
  readResponse (StringBody s) = lmap show $ SimpleJson.readJSON s
  readResponse _ = Left "Invalid response type, expected String"
else instance readResponseDefault :: ReadResponse any where
  readResponse _ = Left "Could not decode response - no ReadResponse instance"
