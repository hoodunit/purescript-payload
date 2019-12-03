module Payload.Client.Queryable where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Payload.Client.DecodeResponse (class DecodeResponse, DecodeResponseError, decodeResponse)
import Payload.Client.EncodeBody (class EncodeBody, encodeBody)
import Payload.Client.Internal.Query (class EncodeQuery, encodeQuery)
import Payload.Client.Internal.Url as PayloadUrl
import Payload.Client.Options (Options, RequestOptions)
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.Internal.Route (DefaultRouteSpec, Undefined)
import Payload.ResponseTypes (Response(..), ResponseBody(..), HttpStatus)
import Payload.Spec (Route)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Record as Record
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow, RLProxy(..))

type ClientFnWithOptions payload body = RequestOptions -> ClientFn payload body
type ClientFn payload body = payload -> Aff (ClientResponse body)
type ClientResponse body = Either ClientError (Response body)
data ClientError
  = DecodeError { error :: DecodeResponseError, response :: Response String }
  | StatusError { response :: Response String }
  | RequestError { message :: String }

instance showClientError :: Show ClientError where
  show (DecodeError err) = "DecodeError: " <> show err
  show (StatusError err) = "StatusError: " <> show err
  show (RequestError err) = "RequestError: " <> show err
instance eqClientError :: Eq ClientError where
  eq (DecodeError a) (DecodeError b) = a == b
  eq (StatusError a) (StatusError b) = a == b
  eq (RequestError a) (RequestError b) = a == b
  eq _ _ = false

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
             -> ClientFnWithOptions payload res

instance queryableGetRoute ::
       ( Row.Lacks "body" route
       , Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , query :: query
           | r }
       , Row.Union baseParams params fullUrlParams
       , Symbol.Append basePath path fullPath

       , RowToList fullUrlParams fullParamsList
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeOptionalQuery fullPath query payload
       , DecodeResponse res
       )
    => Queryable (Route "GET" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let urlPath = encodeUrlWithParams opts
                        (SProxy :: _ fullPath)
                        (RLProxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeOptionalQuery (SProxy :: _ fullPath)
                               (Proxy :: _ query)
                               payload
    let url = urlPath <> urlQuery
    let defaultReq = AX.defaultRequest
          { method = Left GET
          , url = url
          , responseFormat = ResponseFormat.string }
    let req = applyReqOpts reqOpts defaultReq
    res <- AX.request req
    pure (decodeAffjaxResponse res)
else instance queryablePostRoute ::
       ( Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , query :: query
           , body :: body
           | r }
       , Row.Union baseParams params fullUrlParams
       , Symbol.Append basePath path fullPath
       , TypeEquals (Record payload)
           { body :: body
           | rest }

       , RowToList fullUrlParams fullParamsList
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeOptionalQuery fullPath query payload
       , DecodeResponse res
       , EncodeBody body
       )
    => Queryable (Route "POST" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let urlPath = encodeUrlWithParams opts
                        (SProxy :: _ fullPath)
                        (RLProxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeOptionalQuery (SProxy :: _ fullPath)
                               (Proxy :: _ query)
                               payload
    let url = urlPath <> urlQuery
    let (body :: body) = Record.get (SProxy :: SProxy "body") (to payload)
    let encodedBody = RequestBody.String (encodeBody body)
    let defaultReq = AX.defaultRequest
          { method = Left POST
          , url = url
          , content = Just encodedBody
          , responseFormat = ResponseFormat.string }
    let req = applyReqOpts reqOpts defaultReq
    res <- AX.request req
    pure (decodeAffjaxResponse res)
else instance queryableHeadRoute ::
       ( Row.Lacks "body" route
       , Row.Lacks "response" route
       , Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { params :: Record params
           , query :: query
           | r }
       , Symbol.Append basePath path fullPath
       , Row.Union baseParams params fullUrlParams

       , RowToList fullUrlParams fullParamsList
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeOptionalQuery fullPath query payload
       )
    => Queryable (Route "HEAD" path (Record route)) basePath baseParams (Record payload) String where
  request _ _ _ opts reqOpts payload = do
    let urlPath = encodeUrlWithParams opts
                        (SProxy :: _ fullPath)
                        (RLProxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeOptionalQuery (SProxy :: _ fullPath)
                               (Proxy :: _ query)
                               payload
    let url = urlPath <> urlQuery
    let defaultReq = AX.defaultRequest
          { method = Left HEAD
          , url = url
          , responseFormat = ResponseFormat.string }
    let req = applyReqOpts reqOpts defaultReq
    res <- AX.request req
    pure (decodeAffjaxResponse res)
else instance queryablePutRoute ::
       ( Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , query :: query
           , body :: body
           | r }
       , Row.Union baseParams params fullUrlParams
       , Symbol.Append basePath path fullPath

       , RowToList fullUrlParams fullParamsList
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeOptionalQuery fullPath query payload
       , EncodeOptionalBody body payload
       , DecodeResponse res
       )
    => Queryable (Route "PUT" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let body = encodeOptionalBody (Proxy :: _ body) payload
    let urlPath = encodeUrlWithParams opts
                        (SProxy :: _ fullPath)
                        (RLProxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeOptionalQuery (SProxy :: _ fullPath)
                               (Proxy :: _ query)
                               payload
    let url = urlPath <> urlQuery
    let defaultReq = AX.defaultRequest
          { method = Left PUT
          , url = url
          , content = body
          , responseFormat = ResponseFormat.string }
    let req = applyReqOpts reqOpts defaultReq
    res <- AX.request req
    pure (decodeAffjaxResponse res)
else instance queryableDeleteRoute ::
       ( Row.Union route DefaultRouteSpec mergedRoute
       , Row.Nub mergedRoute routeWithDefaults
       , TypeEquals (Record routeWithDefaults)
           { response :: res
           , params :: Record params
           , query :: query
           , body :: body
           | r }

       , Row.Union baseParams params fullUrlParams
       , Symbol.Append basePath path fullPath

       , DecodeResponse res

       , RowToList fullUrlParams fullParamsList
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeOptionalQuery fullPath query payload
       , EncodeOptionalBody body payload
       )
    => Queryable (Route "DELETE" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let body = encodeOptionalBody (Proxy :: _ body) payload
    let urlPath = encodeUrlWithParams opts
                        (SProxy :: _ fullPath)
                        (RLProxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeOptionalQuery (SProxy :: _ fullPath)
                               (Proxy :: _ query)
                               payload
    let url = urlPath <> urlQuery
    let defaultReq = AX.defaultRequest
          { method = Left DELETE
          , url = url
          , content = body
          , responseFormat = ResponseFormat.string }
    let req = applyReqOpts reqOpts defaultReq
    res <- AX.request req
    pure (decodeAffjaxResponse res)

decodeAffjaxResponse :: forall body
  . DecodeResponse body
  => Either AX.Error (AX.Response String)
  -> ClientResponse body
decodeAffjaxResponse (Left err) = Left (RequestError { message: AX.printError err })
decodeAffjaxResponse (Right res@{ status: StatusCode s }) | s >= 200 && s < 300 = do
  case decodeResponse (StringBody res.body) of
    Right decoded -> Right (bodyResponse res decoded)
    Left err -> Left (decodeError res err)
decodeAffjaxResponse (Right res) = Left (StatusError { response: asPayloadResponse res })

decodeError :: AX.Response String -> DecodeResponseError -> ClientError
decodeError res error = DecodeError { error, response: asPayloadResponse res }

bodyResponse :: forall a. AX.Response String -> a -> Response a
bodyResponse res body = Response (Record.insert (SProxy :: _ "body") body rest)
  where
    rest = statusAndHeaders res

asPayloadResponse :: AX.Response String
                 -> Response String
asPayloadResponse res = Response (Record.insert (SProxy :: _ "body") res.body rest)
  where
    rest = statusAndHeaders res

statusAndHeaders :: forall a. AX.Response a
                 -> { status :: HttpStatus, headers :: Headers }
statusAndHeaders res = { status, headers }
  where
    status = { code: unwrapStatus res.status, reason: res.statusText }
    headers = Headers.fromFoldable (asHeaderTuple <$> res.headers)
    unwrapStatus (StatusCode code) = code

asHeaderTuple :: ResponseHeader -> Tuple String String
asHeaderTuple (ResponseHeader name value) = Tuple name value

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

class EncodeOptionalQuery
  (url :: Symbol)
  (query :: Type)
  (payload :: # Type)
  where
    encodeOptionalQuery :: SProxy url
                  -> Proxy query
                  -> Record payload
                  -> String

-- Still need to encode here in case of query literals
instance encodeOptionalQueryUndefined ::
  ( EncodeQuery url ()
  ) => EncodeOptionalQuery url Undefined payload where
  encodeOptionalQuery url _ payload = encodeQuery url {}
else instance encodeOptionalQueryDefined ::
  ( TypeEquals (Record payload) { query :: Record query | rest }
  , EncodeQuery url query
  ) => EncodeOptionalQuery url (Record query) payload where
  encodeOptionalQuery url _ payload = encodeQuery url (to payload).query

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

applyReqOpts :: forall a. RequestOptions -> AX.Request a -> AX.Request a
applyReqOpts { headers } req = req { headers = newHeaders }
  where
    newHeaders = req.headers <> (asAxHeader <$> Headers.toUnfoldable headers)

    asAxHeader :: Tuple String String -> RequestHeader
    asAxHeader (Tuple key val) = RequestHeader key val
