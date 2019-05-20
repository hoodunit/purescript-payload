module Payload.Examples.Basic.Api where

import Data.List (List)
import Payload.Guards (Guard(..))
import Payload.Handlers (File(..))
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
  { getUsers: Route :: GET "/users"
      { guards :: { adminUser :: AdminUser }
      , response :: Array User }
  , getUsersNonAdmin: Route :: GET "/users"
      { response :: Array User }
  , getUser: Route :: GET "/users/<id>"
      { params :: { id :: Int }
      , response :: User }
  , getUsersProfiles: Route :: GET "/users/profiles"
      { response :: Array String }
  , createUser: Route :: POST "/users/new"
      { body :: User
      , guards :: { adminUser :: AdminUser }
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
