module Payload.Method where

import Prelude

data Method
  = DELETE
  | GET
  | HEAD
  | OPTIONS
  | POST
  | PUT

instance showMethod :: Show Method where
  show DELETE = "DELETE"
  show GET = "GET"
  show HEAD = "HEAD"
  show OPTIONS = "OPTIONS"
  show POST = "POST"
  show PUT = "PUT"
