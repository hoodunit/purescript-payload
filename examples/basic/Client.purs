module Payload.Examples.Basic.Client where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Client as Client
import Payload.Examples.Basic.Api (api)

main :: Effect Unit
main = launchAff_ do
  users <- Client.request api.getUsers {}
  liftEffect $ log (show users)
