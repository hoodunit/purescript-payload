module Payload.Client.Queryable where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType (MediaType(..))
import Data.String (Pattern(..), joinWith, stripSuffix) as String
import Data.String.Utils (startsWith) as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Payload.Client.DecodeResponse (class DecodeResponse, DecodeResponseError(..), decodeResponse)
import Payload.Client.EncodeBody (class EncodeBody, encodeBody)
import Payload.Client.Fetch (FetchOptions, FetchResponse, fetch)
import Payload.Client.Fetch as Fetch
import Payload.Client.Internal.Query (class EncodeQuery, encodeQuery)
import Payload.Client.Internal.Url as PayloadUrl
import Payload.Client.Options (LogLevel(..), Options, RequestOptions)
import Payload.Client.Response (ClientError(..), ClientResponse)
import Payload.ContentType (class HasContentType, getContentType)
import Payload.Debug (formatJsonString)
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.Internal.Route (DefaultRouteSpec, Undefined)
import Payload.Method (Method(..))
import Payload.ResponseTypes (Response(..), ResponseBody(..), HttpStatus)
import Payload.Spec (Route)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Record as Record
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)

type ClientFnWithOptions payload body = RequestOptions -> ClientFn payload body
type ClientFn payload body = payload -> Aff (ClientResponse body)

class Queryable
  route
  (basePath :: Symbol)
  (baseParams :: Row Type)
  payload
  res
  | route baseParams basePath -> payload, route -> res where
  request :: route
             -> Proxy basePath
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
                        (Proxy :: _ fullPath)
                        (Proxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeOptionalQuery (Proxy :: _ fullPath)
                               (Proxy :: _ query)
                               payload
    let url = urlPath <> urlQuery
    makeRequest {method: GET, url, body: Nothing, headers: Headers.empty, opts, reqOpts}
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

       , RowToList fullUrlParams fullParamsList
       , EncodeUrlWithParams fullPath fullParamsList payload
       , EncodeOptionalQuery fullPath query payload
       , EncodeOptionalBody body payload
       , HasContentType body
       , DecodeResponse res
       )
    => Queryable (Route "POST" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let urlPath = encodeUrlWithParams opts
                        (Proxy :: _ fullPath)
                        (Proxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeOptionalQuery (Proxy :: _ fullPath)
                               (Proxy :: _ query)
                               payload
    let url = urlPath <> urlQuery
    let body = encodeOptionalBody (Proxy :: _ body) payload
    let headers = maybe Headers.empty (\_ -> Headers.fromFoldable [Tuple "Content-Type" (getContentType (Proxy :: _ body))]) body
    makeRequest {method: POST, url, body, headers, opts, reqOpts}
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
                        (Proxy :: _ fullPath)
                        (Proxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeOptionalQuery (Proxy :: _ fullPath)
                               (Proxy :: _ query)
                               payload
    let url = urlPath <> urlQuery
    makeRequest {method: HEAD, url, body: Nothing, headers: Headers.empty, opts, reqOpts}
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
       , HasContentType body
       , DecodeResponse res
       )
    => Queryable (Route "PUT" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let urlPath = encodeUrlWithParams opts
                        (Proxy :: _ fullPath)
                        (Proxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeOptionalQuery (Proxy :: _ fullPath)
                               (Proxy :: _ query)
                               payload
    let url = urlPath <> urlQuery
    let body = encodeOptionalBody (Proxy :: _ body) payload
    let headers = maybe Headers.empty (\_ -> Headers.fromFoldable [Tuple "Content-Type" (getContentType (Proxy :: _ body))]) body
    makeRequest {method: PUT, url, body, headers, opts, reqOpts}
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
       , HasContentType body
       )
    => Queryable (Route "DELETE" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let body = encodeOptionalBody (Proxy :: _ body) payload
    let urlPath = encodeUrlWithParams opts
                        (Proxy :: _ fullPath)
                        (Proxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeOptionalQuery (Proxy :: _ fullPath)
                               (Proxy :: _ query)
                               payload
    let url = urlPath <> urlQuery
    let headers = maybe Headers.empty (\_ -> Headers.fromFoldable [Tuple "Content-Type" (getContentType (Proxy :: _ body))]) body
    makeRequest {method: DELETE, url, body, headers, opts, reqOpts}
else instance queryableOptionsRoute ::
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
    => Queryable (Route "OPTIONS" path (Record route)) basePath baseParams (Record payload) res where
  request _ _ _ opts reqOpts payload = do
    let urlPath = encodeUrlWithParams opts
                        (Proxy :: _ fullPath)
                        (Proxy :: _ fullParamsList)
                        payload
    let urlQuery = encodeOptionalQuery (Proxy :: _ fullPath)
                               (Proxy :: _ query)
                               payload
    let url = urlPath <> urlQuery
    makeRequest {method: OPTIONS, url, body: Nothing, headers: Headers.empty, opts, reqOpts}

type Request =
  { method :: Method
  , url :: String
  , body :: Maybe String
  , headers :: Headers
  , opts :: Options
  , reqOpts :: RequestOptions }

makeRequest :: forall body. DecodeResponse body => Request -> Aff (ClientResponse body)
makeRequest {method, url, body, headers, opts, reqOpts} = do
  case opts.logLevel of
    LogDebug -> liftEffect (log (printRequest url fetchOptions))
    _ -> pure unit
  res <- fetch url fetchOptions
  case opts.logLevel of
    LogDebug -> liftEffect (log (printResponse res))
    _ -> pure unit
  decodeFetchResponse res
  where
    fetchOptions :: FetchOptions
    fetchOptions =
      { method
      , body
      , headers: headers <> opts.extraHeaders <> reqOpts.extraHeaders }

printRequest :: forall fmt. String -> FetchOptions -> String
printRequest url {method, headers, body} =
  "DEBUG Request:\n" <>
  "--------------------------------\n" <>
  show method <> " " <> url <> "\n" <>
  printHeaders (Headers.toUnfoldable headers) <> "\n" <>
  fromMaybe "" body <> "\n" <>
  "--------------------------------\n"
  where
    printHeaders :: Array (Tuple String String) -> String
    printHeaders [] = ""
    printHeaders hdrs = headersStr <> "\n"
       where
         headersStr = String.joinWith "  \n" (printHeader <$> hdrs)

    printHeader :: Tuple String String -> String
    printHeader (Tuple name value) = name <> ": " <> value

printResponse :: forall fmt. Either Error FetchResponse -> String
printResponse (Left error) =
  "DEBUG Response:\n" <>
  "--------------------------------\n" <>
  show error <>
  "--------------------------------\n"
printResponse (Right {status, headers}) =
  "DEBUG Response:\n" <>
  "--------------------------------\n" <>
  "Status: " <> show status.code <> " " <> status.reason <> "\n" <>
  "Headers:\n" <> printHeaders (Headers.toUnfoldable headers) <> "\n" <>
  -- "Body:\n" <> printBody body <> "\n" <>
  "--------------------------------\n"
  where
    printHeaders :: Array (Tuple String String) -> String
    printHeaders [] = ""
    printHeaders hdrs = headersStr <> "\n"
       where
         headersStr = String.joinWith "  \n" (printHeader <$> hdrs)

    printHeader :: Tuple String String -> String
    printHeader (Tuple name value) = name <> ": " <> value

decodeFetchResponse :: forall body
  . DecodeResponse body
  => Either Error FetchResponse
  -> Aff (ClientResponse body)
decodeFetchResponse (Left err) = pure (Left (RequestError { message: show err }))
decodeFetchResponse (Right res@{ status, headers }) | status.code >= 200 && status.code < 300 = do
  decoded <- decodeResponse res
  case decoded of
    Right decoded -> pure $ Right (bodyResponse res decoded)
    Left error@(JsonDecodeError { body, errors }) -> do
      pure $ Left $ DecodeError {error, response: Response {status, headers, body}}
    Left err -> do
      decodedErr <- decodeError res err
      pure $ Left decodedErr
-- Non-200 statuses are treated as errors
decodeFetchResponse (Right res) = do
  response <- asPayloadResponse res
  pure (Left (StatusError { response }))

decodeError :: FetchResponse -> DecodeResponseError -> Aff ClientError
decodeError res error = do
  response <- asPayloadResponse res
  pure (DecodeError { error, response })

bodyResponse :: forall a. FetchResponse -> a -> Response a
bodyResponse {status, headers} body = Response (Record.insert (Proxy :: _ "body") body {status, headers})

asPayloadResponse :: FetchResponse -> Aff (Response String)
asPayloadResponse {status, headers, raw} = do
  bodyResp <- Fetch.text raw
  case bodyResp of
    Right body -> pure (Response (Record.insert (Proxy :: _ "body") body {status, headers}))
    Left err -> pure (Response (Record.insert (Proxy :: _ "body") "" {status, headers}))

class EncodeOptionalBody
  (body :: Type)
  (payload :: Row Type)
  where
    encodeOptionalBody :: Proxy body
                  -> Record payload
                  -> Maybe String

instance encodeOptionalBodyUndefined :: EncodeOptionalBody Undefined payload where
  encodeOptionalBody _ payload = Nothing
else instance encodeOptionalBodyDefined ::
  ( TypeEquals (Record payload) { body :: body | rest }
  , EncodeBody body
  ) => EncodeOptionalBody body payload where
  encodeOptionalBody _ payload = Just $ encodeBody (to payload).body

class EncodeOptionalQuery
  (url :: Symbol)
  (query :: Type)
  (payload :: Row Type)
  where
    encodeOptionalQuery :: Proxy url
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
  (params :: RowList Type)
  (payload :: Row Type)
  where
    encodeUrlWithParams ::
                  Options
                  -> Proxy url
                  -> Proxy params
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
  => Options -> Proxy url -> Record params -> String
encodeUrl opts url params =
  baseUrl <> path
  where
    path = PayloadUrl.encodeUrl url params
    baseUrl = stripTrailingSlash opts.baseUrl

stripTrailingSlash :: String -> String
stripTrailingSlash s = case String.stripSuffix (String.Pattern "/") s of
  Just stripped -> stripped
  Nothing -> s
