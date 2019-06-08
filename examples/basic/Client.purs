module Payload.Examples.Basic.Client where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Client (mkClient)
import Payload.Client as Client
import Payload.Examples.Basic.Api (apiStructured)

main :: Effect Unit
main = launchAff_ do
  let client = mkClient Client.defaultOpts apiStructured
  users <- client.adminUsers.getUsers identity {}
  liftEffect $ log (show users)
