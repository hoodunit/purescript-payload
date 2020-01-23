module Payload.Docs where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Payload.Docs.DocumentedApi (class DocumentedApi)
import Payload.Docs.DocumentedApi as DocumentedApi
import Payload.Docs.OpenApi (OpenApiSpec)
import Payload.Docs.OpenApi as OpenApi
import Payload.Headers as Headers
import Payload.ResponseTypes (Response(..))
import Payload.Server.Response as Response
import Payload.Spec (type (:), GET, Spec, Tags(..), Nil)
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

htmlPage :: OpenApiSpec -> String
htmlPage openApiSpec = """<!DOCTYPE html>
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
    <div id="redoc" />
    <script type="application/json" id="openApiSpec">""" <> toJson openApiSpec <> """</script>
    <script src="https://cdn.jsdelivr.net/npm/redoc@next/bundles/redoc.standalone.js"> </script>
    <script>
      var openApiSpec = JSON.parse(document.getElementById('openApiSpec').innerHTML);
      var redocElem = document.getElementById('redoc');
      Redoc.init(openApiSpec, {}, redocElem);
    </script>
  </body>
</html>"""

type DocsEndpoint path = GET path {
  summary :: SProxy "API Documentation",
  description :: SProxy "View API documentation page. API documentation is generated at run-time based on the server spec,\
                        \ so docs are always in sync with the code.",
  tags :: Tags ("Documentation" : Nil),
  response :: String
}

docsHandler :: forall routesSpec r
               . DocumentedApi routesSpec
               => Options
               -> Spec { routes :: routesSpec | r }
               -> {}
               -> Aff (Response String)
docsHandler opts spec = let openApiSpec = mkOpenApiSpec opts spec in
  \_ -> do
    let response = Response.ok (htmlPage openApiSpec)
                     # Response.setHeaders (Headers.fromFoldable [Tuple "content-type" "text/html"])
    pure response

docsHandler_ :: forall routesSpec r
               . DocumentedApi routesSpec
               => Spec { routes :: routesSpec | r }
               -> {}
               -> Aff (Response String)
docsHandler_ = docsHandler defaultOpts
