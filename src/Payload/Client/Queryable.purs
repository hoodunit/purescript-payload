module Payload.Client.Queryable where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method(..), unCustomMethod)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.String (Pattern(..), joinWith, stripSuffix) as String
import Data.String.Utils (startsWith) as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Payload.Client.DecodeResponse (class DecodeResponse, DecodeResponseError, decodeResponse)
import Payload.Client.EncodeBody (class EncodeBody, encodeBody)
import Payload.Client.Internal.Query (class EncodeQuery, encodeQuery)
import Payload.Client.Internal.Url as PayloadUrl
import Payload.Client.Options (LogLevel(..), Options, RequestOptions)
import Payload.Client.Response (ClientError(..), ClientResponse)
import Payload.ContentType (class HasContentType, getContentType)
import Payload.Debug (formatJsonString)
import Payload.Driver (getDriver)
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.Internal.Route (DefaultRouteSpec, Undefined)
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
    makeRequest {method: GET, url, body: Nothing, headers: [], opts, reqOpts}
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
    let headers = maybe [] (\_ -> [ContentType (MediaType (getContentType (Proxy :: _ body)))]) body
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
    makeRequest {method: HEAD, url, body: Nothing, headers: [], opts, reqOpts}
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
    let headers = maybe [] (\_ -> [ContentType (MediaType (getContentType (Proxy :: _ body)))]) body
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
    let headers = maybe [] (\_ -> [ContentType (MediaType (getContentType (Proxy :: _ body)))]) body
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
    makeRequest {method: OPTIONS, url, body: Nothing, headers: [], opts, reqOpts}

type Request =
  { method :: Method
  , url :: String
  , body :: Maybe RequestBody.RequestBody
  , headers :: Array RequestHeader
  , opts :: Options
  , reqOpts :: RequestOptions }

makeRequest :: forall body. DecodeResponse body => Request -> Aff (ClientResponse body)
makeRequest {method, url, body, headers, opts, reqOpts} = do
  case opts.logLevel of
    LogDebug -> liftEffect (log (printRequest req))
    _ -> pure unit
  driver <- liftEffect getDriver
  res <- AX.request driver req
  case opts.logLevel of
    LogDebug -> liftEffect (log (printResponse res))
    _ -> pure unit
  pure (decodeAffjaxResponse res)
  where
    defaultReq = AX.defaultRequest
      { method = Left method
      , url = url
      , content = body
      , responseFormat = ResponseFormat.string
      , headers = AX.defaultRequest.headers <> headers
      }
    req = appendHeaders (opts.extraHeaders <> reqOpts.extraHeaders) defaultReq

printRequest :: AX.Request String -> String
printRequest {method, url, headers, content} =
  "DEBUG Request:\n" <>
  "--------------------------------\n" <>
  printMethod method <> " " <> url <> "\n" <>
  printHeaders headers <>
  printContent content <>
  "--------------------------------\n"
  where
    printMethod :: Either Method CustomMethod -> String
    printMethod (Left m) = show m
    printMethod (Right m) = unCustomMethod m

    printHeaders :: Array RequestHeader -> String
    printHeaders [] = ""
    printHeaders hdrs = headersStr <> "\n"
       where
         headersStr = String.joinWith "  \n" (printHeader <$> hdrs)

    printHeader :: RequestHeader -> String
    printHeader (Accept mediaType) = "accept " <> show mediaType
    printHeader (ContentType mediaType) = "content-type " <> show mediaType
    printHeader (RequestHeader key val) = key <> " " <> val

    printContent :: Maybe RequestBody.RequestBody -> String
    printContent (Just (RequestBody.String s)) = s <> "\n"
    printContent (Just _) = "(non-String body)\n"
    printContent Nothing = ""

printResponse :: Either AX.Error (AX.Response String) -> String
printResponse (Left error) =
  "DEBUG Response:\n" <>
  "--------------------------------\n" <>
  AX.printError error <>
  "--------------------------------\n"
printResponse (Right {status, statusText, headers, body}) =
  "DEBUG Response:\n" <>
  "--------------------------------\n" <>
  "Status: " <> printStatus status <> " " <> statusText <> "\n" <>
  "Headers:\n" <> printHeaders headers <> "\n" <>
  "Body:\n" <> printBody body <> "\n" <>
  "--------------------------------\n"
  where
    printStatus :: StatusCode -> String
    printStatus (StatusCode code) = show code
    
    printHeaders :: Array ResponseHeader -> String
    printHeaders [] = ""
    printHeaders hdrs = (String.joinWith "  \n" (printHeader <$> hdrs)) <> "\n"

    contentIsJson = maybe false (String.startsWith "application/json") $ lookupHeader "content-type" headers

    printHeader :: ResponseHeader -> String
    printHeader (ResponseHeader field val) = field <> " " <> val

    printBody :: String -> String
    printBody b | contentIsJson = formatJsonString b
                | otherwise = b

lookupHeader :: String -> Array ResponseHeader -> Maybe String
lookupHeader key headers = Array.findMap matchingHeaderVal headers
  where
    matchingHeaderVal :: ResponseHeader -> Maybe String
    matchingHeaderVal (ResponseHeader k val) | k == "content-type" = Just val
                                             | otherwise = Nothing

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
bodyResponse res body = Response (Record.insert (Proxy :: _ "body") body rest)
  where
    rest = statusAndHeaders res

asPayloadResponse :: AX.Response String
                 -> Response String
asPayloadResponse res = Response (Record.insert (Proxy :: _ "body") res.body rest)
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
  (payload :: Row Type)
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

appendHeaders :: forall a. Headers -> AX.Request a -> AX.Request a
appendHeaders headers req = req { headers = newHeaders }
  where
    newHeaders = req.headers <> (asAxHeader <$> Headers.toUnfoldable headers)

    asAxHeader :: Tuple String String -> RequestHeader
    asAxHeader (Tuple key val) = RequestHeader key val
