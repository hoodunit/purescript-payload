module Payload.Examples.Basic.Main where

import Prelude

import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Node.HTTP as HTTP
import Payload.Examples.Basic.Spec (AdminUser(..), Post, User)
import Payload.Headers as Headers
import Payload.ResponseTypes (Failure(Forward))
import Payload.Server.Guards as Guards
import Payload.Server.Handlers as Handlers

getUsers :: { guards :: { adminUser :: AdminUser, request :: HTTP.Request }} -> Aff (Array User)
getUsers { guards: { adminUser: AdminUser adminUser } } = pure [adminUser, { id: 1, name: "John Doe" }]

getUsersNonAdmin :: { params :: { name :: String } } -> Aff (Array User)
getUsersNonAdmin _ = pure [{ id: 1, name: "John Doe" }]

getUser :: { params :: { id :: Int } } -> Aff User
getUser {params: {id}} = pure { id, name: "whodunnit" }

getUsersProfiles :: {} -> Aff (Array String)
getUsersProfiles _ = pure ["Profile1", "Profile2"]

createUser :: forall r. { body :: User | r } -> Aff User
createUser {body: user} = pure user

getUserPost :: { params :: { id :: Int, postId :: String } } -> Aff Post
getUserPost {params: {postId}} = pure { id: postId, text: "Some post" }

getPage :: { params :: { id :: String } } -> Aff String
getPage { params: {id} } = pure $ "Page " <> id

getPageMetadata :: { params :: { id :: String } } -> Aff String
getPageMetadata { params: {id} } = pure $ "Page metadata " <> id

getHello :: {} -> Aff String
getHello _ = pure "Hello!"

search :: { query :: { a :: Int, b :: Int, rest :: Object (Array String) } }
          -> Aff String
search _ = pure "Search result"

getAdminUser :: HTTP.Request -> Aff (Either Failure AdminUser)
getAdminUser req = do
  authTokenRes <- parseAuthToken req
  case authTokenRes of
    Right token -> pure (Right (AdminUser { id: 1, name: "John Admin" }))
    Left err -> pure (Left (Forward "Not an admin"))

parseAuthToken :: HTTP.Request -> Aff (Either String String)
parseAuthToken req = do
  headers <- Guards.headers req
  case Headers.lookup "authorization" headers of
    Just header -> do
      let tokenSuffix = String.stripPrefix (String.Pattern "Token ") header
      case tokenSuffix of
        Just token -> pure (Right token)
        Nothing -> pure (Left "Couldn't get token")
    Nothing -> pure (Left "No auth header found")

api = {
  handlers: {
    users: {
       getProfiles: getUsersProfiles,
       byId: {
         get: getUser,
         getPost: getUserPost
       }
    },
    adminUsers: {
      get: getUsers,
      create: createUser
    },
    getUsersNonAdmin,
    indexPage: \_ -> Handlers.file "test/index.html",
    files: \{params: {path}} -> Handlers.directory "test" path,
    getPage,
    getPageMetadata,
    getHello,
    search
  },
  guards: {
    adminUser: getAdminUser,
    request: Guards.rawRequest
  }
}
