module Payload.Examples.Basic.Client where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Client.Client (mkClient)
import Payload.Client.Client as Client
import Payload.Examples.Basic.Api (spec)

main :: Effect Unit
main = launchAff_ do
  let client = mkClient Client.defaultOpts spec
  users <- client.adminUsers.getUsers identity {}
  liftEffect $ log (show users)
