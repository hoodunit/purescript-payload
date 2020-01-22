module Payload.Docs where

import Prelude

import Data.Maybe (Maybe(..))
import Payload.Docs.DocumentedApi (class DocumentedApi)
import Payload.Docs.DocumentedApi as DocumentedApi
import Payload.Docs.OpenApi (OpenApiSpec)
import Payload.Docs.OpenApi as OpenApi
import Payload.Spec (Spec)
import Simple.JSON as SimpleJSON

type Options =
  { baseUrl :: Maybe String
  , info :: OpenApi.Info }

defaultOpts :: Options
defaultOpts =
  { baseUrl: Nothing
  , info: OpenApi.defaultInfo }

mkOpenApiSpec :: forall routesSpec r
                 . DocumentedApi routesSpec
                 => Options
                 -> Spec { routes :: routesSpec | r }
                 -> OpenApiSpec
mkOpenApiSpec opts spec = routesOpenApiSpec
                            { servers = servers
                            , info = opts.info }
  where
    routesOpenApiSpec :: OpenApiSpec
    routesOpenApiSpec = DocumentedApi.mkOpenApiSpec spec

    servers :: Array OpenApi.Server
    servers = case opts.baseUrl of
      Just url -> [{url, description: Nothing}]
      Nothing -> []

mkOpenApiSpec_ :: forall routesSpec r
                  . DocumentedApi routesSpec
                  => Spec { routes :: routesSpec | r }
                  -> OpenApiSpec
mkOpenApiSpec_ = mkOpenApiSpec defaultOpts

toJson :: OpenApiSpec -> String
toJson openApiSpec = SimpleJSON.writeJSON openApiSpec
