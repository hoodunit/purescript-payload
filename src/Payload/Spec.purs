module Payload.Spec where

import Prelude

data Route (m :: Symbol) (p :: Symbol) spec = Route
type GET = Route "GET"
type HEAD = Route "HEAD"
type POST = Route "POST"
type PUT = Route "PUT"
type DELETE = Route "DELETE"
