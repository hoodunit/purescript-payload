module Payload.Examples.Basic.Client where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Client (mkGuardedClient)
import Payload.Client as Client
import Payload.Examples.Basic.Spec (spec)

main :: Effect Unit
main = launchAff_ do
  let clientOpts = Client.defaultOpts { baseUrl = "http://localhost:3000" }
  let client = mkGuardedClient clientOpts spec
  users <- client.adminUsers.getUsers {}
  liftEffect $ log (show users)
