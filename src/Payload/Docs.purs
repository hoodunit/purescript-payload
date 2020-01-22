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

htmlPage :: String -> String
htmlPage openApiSpecUrl = """<!DOCTYPE html>
<html>
  <head>
    <title>ReDoc</title>
    <!-- needed for adaptive design -->
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link href="https://fonts.googleapis.com/css?family=Montserrat:300,400,700|Roboto:300,400,700" rel="stylesheet">

    <!--
    ReDoc doesn't change outer page styles
    -->
    <style>
      body {
        margin: 0;
        padding: 0;
      }
    </style>
  </head>
  <body>
    <redoc spec-url='""" <> openApiSpecUrl <> """'></redoc>
    <script src="https://cdn.jsdelivr.net/npm/redoc@next/bundles/redoc.standalone.js"> </script>
  </body>
</html>"""
