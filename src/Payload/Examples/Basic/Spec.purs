module Payload.Examples.Basic.Spec where

import Data.List (List)
import Foreign.Object (Object)
import Node.HTTP as HTTP
import Node.Stream (Read, Stream)
import Payload.Spec (type (:), Spec(Spec), GET, Guards, Nil, POST, Routes)

spec :: Spec {
  guards :: {
     adminUser :: AdminUser,
     request :: HTTP.Request
  },
  routes :: {
    users :: Routes "/users" {
      getProfiles :: GET "/profiles" {
        response :: Array String
      },
      byId :: Routes "/<id>" {
        params :: { id :: Int },
        get :: GET "/" {
          response :: User
        },
        getPost :: GET "/posts/<postId>" {
          params :: { postId :: String },
          response :: Post
        }
      }
    },
    adminUsers :: Routes "/users" {
      guards :: Guards ("adminUser" : Nil),
      get :: GET "/" {
        guards :: Guards ("request" : Nil),
        response :: Array User
      },
      create :: POST "/new" {
        body :: User,
        response :: User
      }
    },
    getUsersNonAdmin :: GET "/<name>" {
      params :: { name :: String },
      response :: Array User
    },
    indexPage :: GET "/" {
      response :: Stream (read :: Read)
    },
    files :: GET "/<..path>" {
      params :: { path :: List String },
      response :: Stream (read :: Read)
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
    },
    search :: GET "/search?a=<a>&b=<b>&<..rest>" {
      query :: { a :: Int, b :: Int, rest :: Object (Array String) },
      response :: String
    }
  }
}
spec = Spec

type User =
  { id :: Int
  , name :: String }

type Post =
  { id :: String
  , text :: String }

newtype AdminUser = AdminUser
  { id :: Int
  , name :: String }
