module Payload.Client.Client
       ( mkClient
       , mkClient_
       , module Payload.Client.Options
       ) where

import Payload.Client.ClientApi (class ClientApi, mkClientApi)
import Payload.Client.Options (ModifyRequest, Options, defaultOpts)
import Payload.Spec (Spec)

mkClient :: forall routesSpec client r
            . ClientApi routesSpec client
            => Options
            -> Spec { routes :: routesSpec | r }
            -> client
mkClient = mkClientApi

mkClient_ :: forall routesSpec client r
            . ClientApi routesSpec client
            => Spec { routes :: routesSpec | r }
            -> client
mkClient_ = mkClientApi defaultOpts
