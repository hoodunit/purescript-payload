module Payload.Test.Helpers where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Routing (class Routable)
import Payload.Server as Payload

withServer
  :: forall routesSpec guardsSpec handlers guards
   . Routable routesSpec guardsSpec handlers guards
  => { routes :: routesSpec, guards :: guardsSpec }
  -> { handlers :: handlers, guards :: guards }
  -> Aff Unit
  -> Aff Unit
withServer apiSpec api_ aff = do
  let opts = Payload.defaultOpts { logLevel = Payload.LogError }
  Aff.bracket (Payload.start opts apiSpec api_) completed runAff
  pure unit
  where
    runAff (Left _) = pure unit
    runAff (Right _) = aff
    completed (Left err) = liftEffect $ log ("Could not start server: " <> err)
    completed (Right server) = Payload.close server
