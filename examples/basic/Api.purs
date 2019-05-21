module Payload.Examples.Basic.Api where

import Data.List (List)
import Node.HTTP as HTTP
import Payload.GuardParsing (type (:), GuardTypes(..), Guards(..), Nil)
import Payload.Handlers (File)
import Payload.Route (GET, POST, Route(..))

type User =
  { id :: Int
  , name :: String }

type Post =
  { id :: String
  , text :: String }

newtype AdminUser = AdminUser
  { id :: Int
  , name :: String }

api =
  { guards: GuardTypes :: _ { adminUser :: AdminUser, request :: HTTP.Request }
  , routes:
    { getUsers: Route :: GET "/users"
        { guards :: Guards ("adminUser" : "request" : Nil)
        , response :: Array User }
    , getUsersNonAdmin: Route :: GET "/<name>"
        { params :: { name :: String }
        , response :: Array User }
    , getUser: Route :: GET "/users/<id>"
        { params :: { id :: Int }
        , response :: User }
    , getUsersProfiles: Route :: GET "/users/profiles"
        { response :: Array String }
    , createUser: Route :: POST "/users/new"
        { body :: User
        , guards :: Guards ("adminUser" : Nil)
        , response :: User }
    , getUserPost: Route :: GET "/users/<id>/posts/<postId>"
        { params :: { id :: Int, postId :: String }
        , response :: Post }
    , indexPage: Route :: GET "/"
        { response :: File }
    , files: Route :: GET "/<..path>"
        { params :: { path :: List String }
        , response :: File }
    , getPage: Route :: GET "/pages/<id>"
        { params :: { id :: String }
        , response :: String }
    , getPageMetadata: Route :: GET "/pages/<id>/metadata"
        { params :: { id :: String }
        , response :: String }
    , getHello: Route :: GET "/hello there"
        { response :: String }
    }
  }
