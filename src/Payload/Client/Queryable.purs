module Payload.Client.Queryable where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect.Aff (Aff)
import Payload.Client.DecodeResponse (class DecodeResponse, decodeResponse)
import Payload.Client.EncodeBody (class EncodeBody, encodeBody)
import Payload.Client.Internal.Query (class EncodeQuery, encodeQuery)
import Payload.Client.Internal.Url as PayloadUrl
import Payload.Client.Options (Options, ModifyRequest)
import Payload.Internal.Route (DefaultRouteSpec, DefaultRouteSpecNoBody, Undefined)
import Payload.Response (ResponseBody(..))
import Payload.Spec (Route)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Record as Record
import Simple.JSON as SimpleJson
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow, RLProxy(..))

class Queryable
  route
  (basePath :: Symbol)
  (baseParams :: # Type)
  payload
  res
  | route baseParams basePath -> payload, route -> res where
  request :: route
             -> SProxy basePath
             -> Proxy (Record baseParams)
             -> Options
             -> ModifyRequest
             -> payload
             -> Aff (Either String res)

instance queryableGetRoute ::
       ( Row.Lacks "body" route
       , Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , query :: Record query
           | r }
       , IsSymbol path
       , Symbol.Append basePath path fullPath
       , Row.Union baseParams params fullUrlParams
       , Row.Union fullUrlParams query fullParams

       , RowToList fullUrlParams fullParamsList
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeQuery fullPath query payload
       , DecodeResponse res
       , SimpleJson.ReadForeign res
       )
    => Queryable (Route "GET" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts modifyReq payload = do
    let urlPath = encodeUrlWithParams opts
                        (SProxy :: _ fullPath)
                        (RLProxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeQuery (SProxy :: _ fullPath)
                               (Proxy :: _ (Record query))
                               payload
    let url = urlPath <> urlQuery
    let defaultReq = AX.defaultRequest
          { method = Left GET
          , url = url
          , responseFormat = ResponseFormat.string }
    let req = modifyReq defaultReq
    res <- AX.request req
    pure (decodeAffjaxResponse res)
else instance queryablePostRoute ::
       ( Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , query :: Record query
           , body :: body
           | r }
       , Row.Union baseParams params fullParams
       , TypeEquals (Record payload)
           { body :: body
           | rest }
       , IsSymbol path
       , Symbol.Append basePath path fullPath

       , RowToList fullParams fullParamsList
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeQuery fullPath query payload
       , DecodeResponse res
       , EncodeBody body
       , SimpleJson.ReadForeign res
       )
    => Queryable (Route "POST" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts modifyReq payload = do
    let urlPath = encodeUrlWithParams opts
                        (SProxy :: _ fullPath)
                        (RLProxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeQuery (SProxy :: _ fullPath)
                               (Proxy :: _ (Record query))
                               payload
    let url = urlPath <> urlQuery
    let (body :: body) = Record.get (SProxy :: SProxy "body") (to payload)
    let encodedBody = RequestBody.String (encodeBody body)
    let defaultReq = AX.defaultRequest
          { method = Left POST
          , url = url
          , content = Just encodedBody
          , responseFormat = ResponseFormat.string }
    let req = modifyReq defaultReq
    res <- AX.request req
    pure (decodeAffjaxResponse res)
else instance queryableHeadRoute ::
       ( Row.Lacks "body" route
       , Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { params :: Record params
           , query :: Record query
           | r }
       , IsSymbol path
       , Symbol.Append basePath path fullPath
       , Row.Union baseParams params fullParams

       , RowToList fullParams fullParamsList
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeQuery fullPath query payload
       )
    => Queryable (Route "HEAD" path (Record route)) basePath baseParams (Record payload) String where
  request _ _ _ opts modifyReq payload = do
    let urlPath = encodeUrlWithParams opts
                        (SProxy :: _ fullPath)
                        (RLProxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeQuery (SProxy :: _ fullPath)
                               (Proxy :: _ (Record query))
                               payload
    let url = urlPath <> urlQuery
    let defaultReq = AX.defaultRequest
          { method = Left HEAD
          , url = url
          , responseFormat = ResponseFormat.string }
    let req = modifyReq defaultReq
    res <- AX.request req
    pure (decodeAffjaxResponse res)
else instance queryablePutRoute ::
       ( Row.Union route DefaultRouteSpecNoBody mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , query :: Record query
           , body :: body
           | r }
       , Row.Union baseParams params fullParams
       , IsSymbol path
       , Symbol.Append basePath path fullPath

       , RowToList fullParams fullParamsList
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeQuery fullPath query payload
       , EncodeOptionalBody body payload
       , DecodeResponse res
       , SimpleJson.ReadForeign res
       )
    => Queryable (Route "PUT" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts modifyReq payload = do
    let body = encodeOptionalBody (Proxy :: _ body) payload
    let urlPath = encodeUrlWithParams opts
                        (SProxy :: _ fullPath)
                        (RLProxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeQuery (SProxy :: _ fullPath)
                               (Proxy :: _ (Record query))
                               payload
    let url = urlPath <> urlQuery
    let defaultReq = AX.defaultRequest
          { method = Left PUT
          , url = url
          , content = body
          , responseFormat = ResponseFormat.string }
    let req = modifyReq defaultReq
    res <- AX.request req
    pure (decodeAffjaxResponse res)
else instance queryableDeleteRoute ::
       -- Set and parse out spec defaults
       ( Row.Union route DefaultRouteSpecNoBody mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , query :: Record query
           , body :: body
           | r }

       , Row.Union baseParams params fullParams
       , IsSymbol path
       , Symbol.Append basePath path fullPath

       , DecodeResponse res
       , SimpleJson.ReadForeign res

       , RowToList fullParams fullParamsList
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeQuery fullPath query payload
       , EncodeOptionalBody body payload
       )
    => Queryable (Route "DELETE" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts modifyReq payload = do
    let body = encodeOptionalBody (Proxy :: _ body) payload
    let urlPath = encodeUrlWithParams opts
                        (SProxy :: _ fullPath)
                        (RLProxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeQuery (SProxy :: _ fullPath)
                               (Proxy :: _ (Record query))
                               payload
    let url = urlPath <> urlQuery
    let defaultReq = AX.defaultRequest
          { method = Left DELETE
          , url = url
          , content = body
          , responseFormat = ResponseFormat.string }
    let req = modifyReq defaultReq
    res <- AX.request req
    pure (decodeAffjaxResponse res)

decodeAffjaxResponse :: forall res. DecodeResponse res =>
                 (AX.Response (Either AX.ResponseFormatError String)) -> Either String res
decodeAffjaxResponse res | res.status /= StatusCode 200 = Left $ "Received HTTP " <> show res.status <> "\n" <>
  (either AX.printResponseFormatError identity res.body)
decodeAffjaxResponse res = do
  showingError res.body >>= (StringBody >>> decodeResponse)
  where
    showingError = lmap ResponseFormat.printResponseFormatError

-- | Allows specs to optionally specify a body for HTTP methods where
-- | a body may or may not appear. If a body is specified in the spec,
-- | it is required in client requests (by deriving the type here as
-- | including the body).
class EncodeOptionalBody
  (body :: Type)
  (payload :: # Type)
  where
    encodeOptionalBody :: Proxy body
                  -> Record payload
                  -> Maybe RequestBody.RequestBody

instance encodeOptionalBodyUndefined :: EncodeOptionalBody Undefined payload where
  encodeOptionalBody _ payload = Nothing
else instance encodeOptionalBodyDefined ::
  ( TypeEquals (Record payload) { body :: body | rest }
  , EncodeBody body
  ) => EncodeOptionalBody body payload where
  encodeOptionalBody _ payload = Just $ RequestBody.String $ encodeBody (to payload).body

class EncodeUrlWithParams
  (url :: Symbol)
  (params :: RowList)
  (payload :: # Type)
  where
    encodeUrlWithParams ::
                  Options
                  -> SProxy url
                  -> RLProxy params
                  -> Record payload
                  -> String

instance encodeUrlWithParamsUndefined ::
  ( PayloadUrl.EncodeUrl url ()
  ) => EncodeUrlWithParams url RowList.Nil payload where
  encodeUrlWithParams options url _ payload = encodeUrl options url {}
else instance encodeUrlWithParamsDefined ::
  ( TypeEquals (Record payload) { params :: Record params | rest }
  , ListToRow rl params
  , PayloadUrl.EncodeUrl url params
  ) => EncodeUrlWithParams url rl payload where
  encodeUrlWithParams options url params payload = encodeUrl options url (to payload).params

encodeUrl :: forall url params
  . PayloadUrl.EncodeUrl url params
  => Options -> SProxy url -> Record params -> String
encodeUrl opts url params =
  baseUrl <> path
  where
    path = PayloadUrl.encodeUrl url params
    baseUrl = stripTrailingSlash opts.baseUrl

stripTrailingSlash :: String -> String
stripTrailingSlash s = case String.stripSuffix (String.Pattern "/") s of
  Just stripped -> stripped
  Nothing -> s
