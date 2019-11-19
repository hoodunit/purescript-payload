module Payload.Server.Internal.Request where

import Data.List (List)

type RequestUrl =
  { method :: String
  , path :: List String
  , query :: String }
