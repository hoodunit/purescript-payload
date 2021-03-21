module Payload.Examples.Basic.Client where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Client (defaultOpts, mkGuardedClient, unwrapBody)
import Payload.Examples.Basic.Spec (spec)

main :: Effect Unit
main = launchAff_ do
  let client = mkGuardedClient (defaultOpts { baseUrl = "http://localhost:3000" }) spec
  existingUser <- unwrapBody (client.users.byId.get {params: {id: 1}})
  newUser <- unwrapBody (client.adminUsers.create {body: {id: 2, name: "whodunnit"}})
  liftEffect $ log $ "Existing: " <> show existingUser
  liftEffect $ log $ "New: " <> show newUser
