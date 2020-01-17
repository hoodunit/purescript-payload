module Payload.OpenApi where

import Prelude

import Data.Maybe (Maybe(..))
import Payload.OpenApi.OpenApiSpec (class OpenApiSpec)
import Payload.OpenApi.OpenApiSpec as OpenApiSpec
import Payload.OpenApi.OpenApiTypes (OpenApi, Server)
import Payload.Spec (Spec(..))
import Simple.JSON as SimpleJSON

type Options =
  { baseUrl :: Maybe String }

defaultOpts :: Options
defaultOpts =
  { baseUrl: Nothing }

mkOpenApiSpec :: forall routesSpec r
                 . OpenApiSpec routesSpec
                 => Options
                 -> Spec { routes :: routesSpec | r }
                 -> OpenApi
mkOpenApiSpec opts spec = routesOpenApiSpec { servers = servers }
  where
    routesOpenApiSpec :: OpenApi
    routesOpenApiSpec = OpenApiSpec.mkOpenApiSpec spec

    servers :: Array Server
    servers = case opts.baseUrl of
      Just url -> [{url, description: Nothing}]
      Nothing -> []

mkOpenApiSpec_ :: forall routesSpec r
                  . OpenApiSpec routesSpec
                  => Spec { routes :: routesSpec | r }
                  -> OpenApi
mkOpenApiSpec_ = mkOpenApiSpec defaultOpts

toJson :: OpenApi -> String
toJson openApiSpec = SimpleJSON.writeJSON openApiSpec
