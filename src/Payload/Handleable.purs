module Payload.Handleable
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
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception (Error)
import Effect.Exception as Ex
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.HTTP as HTTP
import Node.Stream (onDataString, onEnd, onError)
import Payload.DecodeBody (class DecodeBody, decodeBody)
import Payload.Guards (class RunGuards, runGuards)
import Payload.Internal.GuardParsing (GuardTypes(..))
import Payload.Internal.GuardParsing as GuardParsing
import Payload.Internal.OmitEmpty (class OmitEmpty, omitEmpty)
import Payload.Internal.Query as PayloadQuery
import Payload.Internal.Request (RequestUrl)
import Payload.Internal.Route (Undefined(..))
import Payload.Internal.Url as PayloadUrl
import Payload.Internal.UrlParsing (class ParseUrl, class ToSegments)
import Payload.Response (Failure(..), RawResponse, Result)
import Payload.Response as Resp
import Payload.Spec (Guards(..), Route, kind GuardList)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Type.Equality (class TypeEquals, to)
import Type.Proxy (Proxy(..))

type MethodHandler = RequestUrl -> HTTP.Request -> HTTP.Response -> Result RawResponse

class Handleable
  route
  handler
  (basePath :: Symbol)
  (baseParams :: # Type)
  (baseGuards :: GuardList)
  (guardsSpec :: # Type)
  guards | route -> handler where
  handle :: SProxy basePath
            -> Proxy (Record baseParams)
            -> Guards baseGuards
            -> GuardTypes (Record guardsSpec)
            -> route
            -> handler
            -> guards
            -> RequestUrl
            -> HTTP.Request
            -> HTTP.Response
            -> Result Resp.RawResponse

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
       , DecodeBody body

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
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec
       )
    => Handleable (Route "POST" path (Record route))
                  (Record payload -> Aff handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards) where
  handle _ _ _ _ route handler allGuards { method, path, query } req res = do
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT Forward $ except $ decodeQuery query
    bodyStr <- lift $ readBody req
    body <- withExceptT Forward $ except $ (decodeBody bodyStr :: Either String body)
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    let (payload :: Record payloadWithEmpty) = to { params, body, query: decodedQuery, guards: guards }
    mkResponse (SProxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (SProxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (SProxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleableRoute ::
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
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec
       )
    => Handleable (Route "GET" path (Record route))
                  (Record payload -> Aff handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards) where
  handle _ _ _ _ route handler allGuards { method, path, query } req res = do
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT Forward $ except $ decodeQuery query
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    let (payload :: Record payloadWithEmpty) = to { params, query: decodedQuery, guards }
    mkResponse (SProxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (SProxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (SProxy :: _ fullPath) (Proxy :: _ (Record query))

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
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec
       )
    => Handleable (Route "HEAD" path (Record route))
                  (Record payload -> Aff handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards) where
  handle _ _ _ _ route handler allGuards { method, path, query } req res = do
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT Forward $ except $ decodeQuery query
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    let (payload :: Record payloadWithEmpty) = to { params, query: decodedQuery, guards }
    Resp.setBody Resp.EmptyBody <$> mkResponse (SProxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (SProxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (SProxy :: _ fullPath) (Proxy :: _ (Record query))

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
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec
       )
    => Handleable (Route "PUT" path (Record route))
                  (Record payload -> Aff handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards) where
  handle _ _ _ _ route handler allGuards { method, path, query } req res = do
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT Forward $ except $ decodeQuery query
    bodyStr <- lift $ readBody req
    body <- withExceptT Forward $ except $ (decodeOptionalBody bodyStr :: Either String body)
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    let (payload :: Record payloadWithEmpty) = to { params, body, query: decodedQuery, guards }
    mkResponse (SProxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (SProxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (SProxy :: _ fullPath) (Proxy :: _ (Record query))

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
       , RunGuards fullGuards guardsSpec allGuards () routeGuardSpec
       )
    => Handleable (Route "DELETE" path (Record route))
                  (Record payload -> Aff handlerRes)
                  basePath
                  baseParams
                  baseGuards
                  guardsSpec
                  (Record allGuards) where
  handle _ _ _ _ route handler allGuards { method, path, query } req res = do
    params <- withExceptT Forward $ except $ decodePath path
    decodedQuery <- withExceptT Forward $ except $ decodeQuery query
    bodyStr <- lift $ readBody req
    body <- withExceptT Forward $ except $ (decodeOptionalBody bodyStr :: Either String body)
    guards <- runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    let (payload :: Record payloadWithEmpty) = to { params, body, query: decodedQuery, guards }
    mkResponse (SProxy :: _ docRoute) (Proxy :: _ res) (handler (omitEmpty payload))

    where
      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (SProxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (SProxy :: _ fullPath) (Proxy :: _ (Record query))

mkResponse :: forall handlerRes res docRoute
  . Resp.ToSpecResponse docRoute handlerRes res
  => Resp.EncodeResponse res
  => SProxy docRoute -> Proxy res -> Aff handlerRes -> Result Resp.RawResponse
mkResponse _ _ aff = do
  (handlerResp :: handlerRes) <- lift $ aff
  (specResp :: Resp.Response res) <- Resp.toSpecResponse (SProxy :: _ docRoute) handlerResp
  (rawResp :: Resp.RawResponse) <- Resp.encodeResponse specResp
  pure rawResp

readBody :: HTTP.Request -> Aff String
readBody req = Aff.makeAff (readBody_ req)

readBody_ :: HTTP.Request -> (Either Error String -> Effect Unit) -> Effect Aff.Canceler
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
