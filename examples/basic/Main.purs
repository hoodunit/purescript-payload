module Payload.Examples.Basic.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.String as String
import Data.String.Utils as StringUtils
import Effect.Aff (Aff)
import Node.HTTP as HTTP
import Payload.Examples.Basic.Api (AdminUser(..), Post, User)
import Payload.Guards as Guards
import Payload.Handlers (File(..))
import Payload.Response (Failure(Forward))

getUsers :: forall r. { adminUser :: AdminUser | r } -> Aff (Array User)
getUsers { adminUser: AdminUser adminUser } = pure [adminUser, { id: 1, name: "John Doe" }]

getUsersNonAdmin :: forall r. { | r } -> Aff (Array User)
getUsersNonAdmin _ = pure [{ id: 1, name: "John Doe" }]

getUser :: forall r. { id :: Int | r } -> Aff User
getUser {id} = pure { id, name: "John Doe" }

getUsersProfiles :: forall r. { | r } -> Aff (Array String)
getUsersProfiles _ = pure ["Profile1", "Profile2"]

createUser :: forall r. { body :: User | r } -> Aff User
createUser {body: user} = pure user

getUserPost :: forall r. { postId :: String | r } -> Aff Post
getUserPost {postId} = pure { id: postId, text: "Some post" }

indexPage :: forall r. { | r} -> Aff File
indexPage _ = pure (File "test/index.html")

-- Exposes to directory traversal attack
files :: forall r. { path :: List String | r} -> Aff File
files { path } = pure (File $ "test/" <> String.joinWith "/" (Array.fromFoldable path))

getPage :: forall r. { id :: String | r} -> Aff String
getPage { id } = pure $ "Page " <> id

getPageMetadata :: forall r. { id :: String | r} -> Aff String
getPageMetadata { id } = pure $ "Page metadata " <> id

getHello :: forall r. { | r} -> Aff String
getHello _ = pure "Hello!"

getAdminUser :: HTTP.Request -> Aff (Either Failure AdminUser)
getAdminUser req = do
  if StringUtils.endsWith "secret" (HTTP.requestURL req)
     then pure (Right (AdminUser { id: 1, name: "John Admin" }))
     else pure (Left (Forward "Not an admin"))

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
