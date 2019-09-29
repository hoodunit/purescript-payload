module Payload.Examples.Basic.Api where

import Data.List (List)
import Node.HTTP as HTTP
import Payload.Handlers (File)
import Payload.Spec (type (:), API(API), GET, Guards, Nil, POST, Routes)

type User =
  { id :: Int
  , name :: String }

type Post =
  { id :: String
  , text :: String }

newtype AdminUser = AdminUser
  { id :: Int
  , name :: String }

api :: API {
  guards :: {
     adminUser :: AdminUser,
     request :: HTTP.Request
  },
  routes :: {
    users :: Routes "/users" {
      getUsersProfiles :: GET "/profiles" {
        response :: Array String
      },
      userById :: Routes "/<id>" {
        params :: { id :: Int },
        getUser :: GET "/" {
          response :: User
        },
        getUserPost :: GET "/posts/<postId>" {
          params :: { postId :: String },
          response :: Post
        }
      }
    },
    adminUsers :: Routes "/users" {
      guards :: Guards ("adminUser" : Nil),
      getUsers :: GET "/" {
        guards :: Guards ("request" : Nil),
        response :: Array User
      },
      createUser :: POST "/new" {
        body :: User,
        response :: User
      }
    },
    getUsersNonAdmin :: GET "/<name>" {
      params :: { name :: String },
      response :: Array User
    },
    indexPage :: GET "/" {
      response :: File
    },
    files :: GET "/<..path>" {
      params :: { path :: List String },
      response :: File
    },
    getPage :: GET "/pages/<id>" {
      params :: { id :: String },
      response :: String
    },
    getPageMetadata :: GET "/pages/<id>/metadata" {
      params :: { id :: String },
      response :: String
    },
    getHello :: GET "/hello there" {
      response :: String
    }
  }
}
api = API
