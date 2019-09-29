module Payload.Spec where

import Prelude

data API apiSpec = API
data Routes (path :: Symbol) routesSpec = Routes
data Route (m :: Symbol) (p :: Symbol) spec = Route

type GET = Route "GET"
type HEAD = Route "HEAD"
type POST = Route "POST"
type PUT = Route "PUT"
type DELETE = Route "DELETE"

data Guards (g :: GuardList) = Guards

foreign import kind GuardList
foreign import data GNil :: GuardList
foreign import data GCons :: Symbol -> GuardList -> GuardList

infixr 1 type GCons as :
type Nil = GNil
