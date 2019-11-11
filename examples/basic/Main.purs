module Payload.Examples.Basic.Main where

import Prelude

import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Utils as StringUtils
import Effect.Aff (Aff)
import Node.HTTP as HTTP
import Payload.Examples.Basic.Spec (AdminUser(..), Post, User)
import Payload.Guards as Guards
import Payload.Handlers (File(..))
import Payload.Handlers as Handlers
import Payload.Headers as Headers
import Payload.Response (Failure(Forward))
import Payload.Response as Response

getUsers :: forall r. { adminUser :: AdminUser | r } -> Aff (Array User)
getUsers { adminUser: AdminUser adminUser } = pure [adminUser, { id: 1, name: "John Doe" }]

getUsersNonAdmin :: forall r. { params :: { name :: String } | r } -> Aff (Array User)
getUsersNonAdmin _ = pure [{ id: 1, name: "John Doe" }]

getUser :: forall r. { params :: { id :: Int } | r } -> Aff User
getUser {params: {id}} = pure { id, name: "John Doe" }

getUsersProfiles :: forall r. { | r } -> Aff (Array String)
getUsersProfiles _ = pure ["Profile1", "Profile2"]

createUser :: forall r. { body :: User | r } -> Aff User
createUser {body: user} = pure user

getUserPost :: forall r. { params :: { id :: String, postId :: String } | r } -> Aff Post
getUserPost {params: {postId}} = pure { id: postId, text: "Some post" }

indexPage :: forall r. { | r} -> Aff File
indexPage _ = pure (File "test/index.html")

files :: forall r. { params :: { path :: List String} | r } -> Aff (Either Failure File)
files { params: {path} } = Handlers.directory "test" path

getPage :: forall r. { params :: { id :: String } | r} -> Aff String
getPage { params: {id} } = pure $ "Page " <> id

getPageMetadata :: forall r. { params :: { id :: String } | r} -> Aff String
getPageMetadata { params: {id} } = pure $ "Page metadata " <> id

getHello :: forall r. { | r} -> Aff String
getHello _ = pure "Hello!"

getAdminUser :: HTTP.Request -> Aff (Either Response.Failure AdminUser)
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
       getUsersProfiles,
       userById: {
         getUser,
         getUserPost
       }
    },
    adminUsers: {
      getUsers,
      createUser
    },
    getUsersNonAdmin,
    indexPage,
    files,
    getPage,
    getPageMetadata,
    getHello
  },
  guards: {
    adminUser: getAdminUser,
    request: Guards.rawRequest
  }
}
