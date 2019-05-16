module Payload.Data where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Simple.JSON as SimpleJson

class FromData data_ where
  fromData :: String -> Either String data_

instance fromDataRecord :: SimpleJson.ReadForeign (Record r) => FromData (Record r) where
  fromData = SimpleJson.readJSON >>> lmap show

instance fromDataArray :: SimpleJson.ReadForeign (Array r) => FromData (Array r) where
  fromData = SimpleJson.readJSON >>> lmap show

instance fromDataString :: FromData String where
  fromData = Right
