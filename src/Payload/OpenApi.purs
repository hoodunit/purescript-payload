module Payload.OpenApi where

import Prelude

import Data.Maybe (Maybe(..))
import Payload.OpenApi.OpenApiSpec (class OpenApiSpec)
import Payload.OpenApi.OpenApiSpec as OpenApiSpec
import Payload.OpenApi.OpenApiTypes as OpenApi
import Payload.Spec (Spec(..))
import Simple.JSON as SimpleJSON

type Options =
  { baseUrl :: Maybe String
  , info :: OpenApi.Info }

defaultOpts :: Options
defaultOpts =
  { baseUrl: Nothing
  , info: OpenApi.defaultInfo }

mkOpenApiSpec :: forall routesSpec r
                 . OpenApiSpec routesSpec
                 => Options
                 -> Spec { routes :: routesSpec | r }
                 -> OpenApi.OpenApi
mkOpenApiSpec opts spec = routesOpenApiSpec
                            { servers = servers
                            , info = opts.info }
  where
    routesOpenApiSpec :: OpenApi.OpenApi
    routesOpenApiSpec = OpenApiSpec.mkOpenApiSpec spec

    servers :: Array OpenApi.Server
    servers = case opts.baseUrl of
      Just url -> [{url, description: Nothing}]
      Nothing -> []

mkOpenApiSpec_ :: forall routesSpec r
                  . OpenApiSpec routesSpec
                  => Spec { routes :: routesSpec | r }
                  -> OpenApi.OpenApi
mkOpenApiSpec_ = mkOpenApiSpec defaultOpts

toJson :: OpenApi.OpenApi -> String
toJson openApiSpec = SimpleJSON.writeJSON openApiSpec
