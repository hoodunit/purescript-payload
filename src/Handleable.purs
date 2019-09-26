module Payload.Handleable where

import Prelude

import Control.Monad.Except (ExceptT(..), except, lift, withExceptT)
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
import Payload.Data (class FromData)
import Payload.Data as Data
import Payload.GuardParsing (GuardTypes(..), Guards(..), kind GuardList)
import Payload.GuardParsing as GuardParsing
import Payload.Guards (class RunGuards, runGuards)
import Payload.Query as PayloadQuery
import Payload.Request (RequestUrl)
import Payload.Response as Resp
import Payload.Route (Route)
import Payload.Url as PayloadUrl
import Payload.UrlParsing (class ParseUrl, class ToSegments)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Record as Record
import Type.Equality (class TypeEquals, from)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type MethodHandler = RequestUrl -> HTTP.Request -> HTTP.Response -> HandlerM Resp.RawResponse
data HandlerFailure = MatchFail String | HandlerError Resp.ServerError
type HandlerM a = ExceptT HandlerFailure Aff a

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
            -> HandlerM Resp.RawResponse

instance handleablePostRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , body :: body
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Resp.ToResponse handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath
       , FromData body

       , Row.Union baseParams params fullUrlParams
       , Row.Union fullUrlParams query fullParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , Row.Union fullParams ( body :: body ) payload'
       , Row.Union payload' routeGuardSpec payload

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
    params <- withExceptT MatchFail $ except $ decodePath path
    decodedQuery <- withExceptT MatchFail $ except $ decodeQuery query
    bodyStr <- lift $ readBody req
    body <- withExceptT MatchFail $ except $ (Data.fromData bodyStr :: Either String body)
    guards <- withExceptT MatchFail $ ExceptT $ runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    let (fullParams :: Record fullParams) = from (Record.union params decodedQuery)
    let (payload' :: Record payload') = Record.union fullParams { body }
    let (payload :: Record payload) = Record.union payload' guards
    handlerResp <- lift $ handler payload
    (specResp :: Resp.Response res) <- pure $ Resp.toResponse handlerResp
    (rawResp :: Resp.RawResponse) <- withExceptT HandlerError $ Resp.encodeResponse specResp
    pure rawResp

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
       , Resp.ToResponse handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath

       , Row.Union baseParams params fullUrlParams
       , Row.Union fullUrlParams query fullParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , Row.Union fullParams routeGuardSpec payload

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
    params <- withExceptT MatchFail $ except $ decodePath path
    decodedQuery <- withExceptT MatchFail $ except $ decodeQuery query
    guards <- withExceptT MatchFail $ ExceptT $ runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    let (fullParams :: Record fullParams) = from (Record.union params decodedQuery)
    let (payload :: Record payload) = from (Record.union fullParams guards)
    handlerResp <- withExceptT HandlerError $ lift $ handler payload
    (specResp :: Resp.Response res) <- pure $ Resp.toResponse handlerResp
    (rawResp :: Resp.RawResponse) <- withExceptT HandlerError $ Resp.encodeResponse specResp
    pure rawResp

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
       , Resp.ToResponse handlerRes res
       , Resp.EncodeResponse res

       , Row.Union baseParams params fullUrlParams
       , Row.Union fullUrlParams query fullParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , Row.Union fullParams routeGuardSpec payload

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
    params <- withExceptT MatchFail $ except $ decodePath path
    decodedQuery <- withExceptT MatchFail $ except $ decodeQuery query
    guards <- withExceptT MatchFail $ ExceptT $ runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    let (fullParams :: Record fullParams) = from (Record.union params decodedQuery)
    let (payload :: Record payload) = from (Record.union fullParams guards)
    handlerResp <- lift $ handler payload
    (specResp :: Resp.Response res) <- pure $ Resp.toResponse handlerResp
    (rawResp :: Resp.RawResponse) <- withExceptT HandlerError $ Resp.encodeResponse specResp
    pure (Resp.setEmptyBody rawResp)

    where
      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (SProxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (SProxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleablePutRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , body :: body
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Resp.ToResponse handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath
       , FromData body

       , Row.Union baseParams params fullUrlParams
       , Row.Union fullUrlParams query fullParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , Row.Union fullParams ( body :: body ) payload'
       , Row.Union payload' routeGuardSpec payload

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
    params <- withExceptT MatchFail $ except $ decodePath path
    decodedQuery <- withExceptT MatchFail $ except $ decodeQuery query
    bodyStr <- lift $ readBody req
    body <- withExceptT MatchFail $ except $ (Data.fromData bodyStr :: Either String body)
    guards <- withExceptT MatchFail $ ExceptT $ runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    let (fullParams :: Record fullParams) = from (Record.union params decodedQuery)
    let (payload' :: Record payload') = Record.union fullParams { body }
    let (payload :: Record payload) = Record.union payload' guards
    handlerResp <- lift $ handler payload
    (specResp :: Resp.Response res) <- pure $ Resp.toResponse handlerResp
    (rawResp :: Resp.RawResponse) <- withExceptT HandlerError $ Resp.encodeResponse specResp
    pure rawResp

    where
      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (SProxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (SProxy :: _ fullPath) (Proxy :: _ (Record query))

instance handleableDeleteRoute ::
       ( TypeEquals (Record route)
           { response :: res
           , params :: Record params
           , guards :: Guards guardNames
           | r }
       , IsSymbol path
       , Resp.ToResponse handlerRes res
       , Resp.EncodeResponse res
       , Symbol.Append basePath path fullPath

       , Row.Union baseParams params fullUrlParams
       , Row.Union fullUrlParams query fullParams
       , PayloadUrl.DecodeUrl fullPath fullUrlParams
       , PayloadQuery.DecodeQuery fullPath query
       , ParseUrl fullPath urlParts
       , ToSegments urlParts

       , Row.Union fullParams routeGuardSpec payload

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
    params <- withExceptT MatchFail $ except $ decodePath path
    decodedQuery <- withExceptT MatchFail $ except $ decodeQuery query
    guards <- withExceptT MatchFail $ ExceptT $ runGuards (Guards :: _ fullGuards) (GuardTypes :: _ (Record guardsSpec)) allGuards {} req
    let (fullParams :: Record fullParams) = from (Record.union params decodedQuery)
    let (payload :: Record payload) = Record.union fullParams guards
    handlerResp <- lift $ handler payload
    (specResp :: Resp.Response res) <- pure $ Resp.toResponse handlerResp
    (rawResp :: Resp.RawResponse) <- withExceptT HandlerError $ Resp.encodeResponse specResp
    pure rawResp

    where
      decodePath :: List String -> Either String (Record fullUrlParams)
      decodePath = PayloadUrl.decodeUrl (SProxy :: _ fullPath) (Proxy :: _ (Record fullUrlParams))

      decodeQuery :: String -> Either String (Record query)
      decodeQuery = PayloadQuery.decodeQuery (SProxy :: _ fullPath) (Proxy :: _ (Record query))

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
