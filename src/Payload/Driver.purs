module Payload.Driver where

import Prelude

import Affjax (AffjaxDriver)
import Affjax.Node as Node
import Affjax.Web as Web
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

foreign import webEnvironment :: Boolean
foreign import globalDriver :: Effect (Nullable AffjaxDriver)

-- | Web or node driver for Affjax
getDriver :: Effect AffjaxDriver
getDriver = do
  driver <- toMaybe <$> globalDriver
  pure $ fromMaybe (if webEnvironment then Web.driver else Node.driver) driver

-- | Set the global Affjax driver to use, in case autodetect fails.
foreign import setDriver :: AffjaxDriver -> Effect Unit
