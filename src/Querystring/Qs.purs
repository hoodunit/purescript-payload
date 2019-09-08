module Payload.Querystring.Qs where

import Prelude

import Data.Map (Map)
import Foreign.Object (Object)

foreign import parse :: String -> Object String
