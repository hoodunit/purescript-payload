module Payload.Server.Handleable
       ( class Handleable
       , MethodHandler
       , handle
       , class DecodeOptionalBody
       , decodeOptionalBody
       ) where

import Prelude

import Control.Monad.Except (except, lift, withExceptT)
import Data.Either (Either(..))
import Data.List (List)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception as Ex
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.HTTP as HTTP
import Node.Stream (onDataString, onEnd, onError)
import Payload.Internal.Route (Undefined(..))
import Payload.Internal.UrlParsing (class ParseUrl, class ToSegments)
import Payload.ResponseTypes (Failure(Error, Forward), RawResponse, Response, ResponseBody(..), Result)
import Payload.Server.DecodeBody (class DecodeBody, decodeBody)
import Payload.Server.Guards (class RunGuards, runGuards)
import Payload.Server.Internal.GuardParsing (GuardTypes(..))
import Payload.Server.Internal.GuardParsing as GuardParsing
import Payload.Server.Internal.OmitEmpty (class OmitEmpty, omitEmpty)
import Payload.Server.Internal.Query as PayloadQuery
import Payload.Server.Internal.Request (RequestUrl)
import Payload.Server.Internal.Url as PayloadUrl
import Payload.Server.Response as Resp
import Payload.Spec (Guards(..), Route, GuardList)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))

type MethodHandler m = RequestUrl -> HTTP.Request -> HTTP.Response -> Result m RawResponse

class Handleable
  route
  handler
  (basePath :: Symbol)
  (baseParams :: Row Type)
  (baseGuards :: GuardList)
  (guardsSpec :: Row Type)
  guards
  m | route -> handler where
  handle :: Proxy basePath
            -> Proxy (Record baseParams)
            -> Guards baseGuards
            -> GuardTypes (Record guardsSpec)
            -> route
            -> handler
            -> guards
            -> RequestUrl
            -> HTTP.Request
            -> HTTP.Response
            -> Result m RawResponse

instance handleablePostRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , body :: body
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append "POST " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath
       , DecodeOptionalBody body

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , body :: body
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec m
       , MonadAff m
       )
    => Handleable (Route "POST" path (Record route))
                  (Record payload -> m handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  m where
  handle _ _ _ _ route handler allGuards { method, path, query } req res = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    bodyStr <- lift $ readBody req
    body <- withExceptT badRequest $ except $ (decodeOptionalBody bodyStr :: Either String body)
    let (payload :: Record payloadWithEmpty) = to { params, body, query: decodedQuery, guards: guards }
    mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleableGetRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append "GET " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec m
       , MonadAff m
       )
    => Handleable (Route "GET" path (Record route))
                  (Record payload -> m handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  m where
  handle _ _ _ _ route handler allGuards { method, path, query } req res = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    let (payload :: Record payloadWithEmpty) = to { params, query: decodedQuery, guards }
    mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleableHeadRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append basePath path fullPath
       , Symbol.Append "HEAD " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec m
       , MonadAff m
       )
    => Handleable (Route "HEAD" path (Record route))
                  (Record payload -> m handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  m where
  handle _ _ _ _ route handler allGuards { method, path, query } req res = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    let (payload :: Record payloadWithEmpty) = to { params, query: decodedQuery, guards }
    Resp.setBody EmptyBody <$> mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleablePutRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , body :: body
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append "PUT " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath
       , DecodeOptionalBody body

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , body :: body
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec m
       , MonadAff m
       )
    => Handleable (Route "PUT" path (Record route))
                  (Record payload -> m handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  m where
  handle _ _ _ _ route handler allGuards { method, path, query } req res = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    bodyStr <- lift $ readBody req
    body <- withExceptT badRequest $ except $ (decodeOptionalBody bodyStr :: Either String body)
    let (payload :: Record payloadWithEmpty) = to { params, body, query: decodedQuery, guards }
    mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleableDeleteRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , body :: body
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append "DELETE " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath
       , DecodeOptionalBody body

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , body :: body
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec m
       , MonadAff m
       )
    => Handleable (Route "DELETE" path (Record route))
                  (Record payload -> m handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  m where
  handle _ _ _ _ route handler allGuards { method, path, query } req res = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    bodyStr <- lift $ readBody req
    body <- withExceptT badRequest $ except $ (decodeOptionalBody bodyStr :: Either String body)
    let (payload :: Record payloadWithEmpty) = to { params, body, query: decodedQuery, guards }
    mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleableOptionsRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , query :: Record query
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Symbol.Append "OPTIONS " fullPath docRoute
       , Resp.ToSpecResponse docRoute handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath

       , Row.Union baseParams params fullUrlParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , TypeEquals
           { query :: Record query
           , params :: Record fullUrlParams
           , guards :: Record routeGuardSpec }
           (Record payloadWithEmpty)
       , OmitEmpty payloadWithEmpty payload

       , GuardParsing.Append baseGuards guardNames fullGuards
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec m
       , MonadAff m
       )
    => Handleable (Route "OPTIONS" path (Record route))
                  (Record payload -> m handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards)
                  m where
  handle _ _ _ _ route handler allGuards { method, path, query } req res = do
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT badRequest $ except $ decodeQuery query
    let (payload :: Record payloadWithEmpty) = to { params, query: decodedQuery, guards }
    mkResponse (Proxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      badRequest :: String -> Failure
      badRequest _ = Error $ Resp.badRequest EmptyBody

      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (Proxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (Proxy :: _ fullPath) (Proxy :: _ (Record query))

mkResponse :: forall handlerRes res docRoute m
  . MonadAff m
  => Resp.ToSpecResponse docRoute handlerRes res
  => Resp.EncodeResponse res
  => Proxy docRoute -> Proxy res -> m handlerRes -> Result m RawResponse
mkResponse _ _ aff = do
  (handlerResp :: handlerRes) <- lift $ aff
  (specResp :: Response res) <- Resp.toSpecResponse (Proxy :: _ docRoute) handlerResp
  (rawResp :: RawResponse) <- Resp.encodeResponse specResp
  pure rawResp

readBody :: forall m. MonadAff m => HTTP.Request -> m String
readBody req = liftAff $ Aff.makeAff (readBody_ req)

readBody_ :: HTTP.Request -> (Either Ex.Error String -> Effect Unit) -> Effect Aff.Canceler
readBody_ req cb = do
  buffer <- Ref.new ""
  let inputStream = HTTP.requestAsStream req
  let handleData str = (flip Ref.modify_) buffer (_ <> str)
  let handleEnd = Ref.read buffer >>= returnBody
  Ex.catchException returnError do
    onError inputStream returnError
    onDataString inputStream UTF8 handleData
    onEnd inputStream handleEnd
  pure mempty
  where
    returnError msg = cb $ Left $ msg
    returnBody val = cb $ Right $ val

class DecodeOptionalBody
  (body :: Type)
  where
    decodeOptionalBody :: String -> Either String body

instance decodeOptionalBodyUndefined :: DecodeOptionalBody Undefined where
  decodeOptionalBody _ = Right Undefined
else instance encodeOptionalBodyDefined ::
  DecodeBody body => DecodeOptionalBody body where
  decodeOptionalBody body = decodeBody body
