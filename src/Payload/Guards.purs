module Payload.Guards where

import Prelude

import Control.Monad.Except (ExceptT(..), lift, runExceptT, throwError)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Node.HTTP as HTTP
import Payload.Cookies as Cookies
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.Internal.GuardParsing (GuardTypes(..))
import Payload.Response as Resp
import Payload.Spec (GCons, GNil, Guards(..), kind GuardList)
import Prim.Row as Row
import Record as Record
import Type.Equality (to)

data Guard (name :: Symbol) a = Guard

type GuardFn a = HTTP.Request -> Aff a

class RunGuards
  (guardNames :: GuardList)
  (guardsSpec :: # Type)
  (allGuards :: # Type)
  (results :: # Type)
  (routeGuardSpec :: # Type) | guardNames guardsSpec allGuards -> routeGuardSpec where
  runGuards :: Guards guardNames
               -> GuardTypes (Record guardsSpec)
               -> Record allGuards
               -> Record results
               -> HTTP.Request
               -> Resp.Result (Record routeGuardSpec)

instance runGuardsNil :: RunGuards GNil guardsSpec allGuards routeGuardSpec routeGuardSpec where
  runGuards _ _ allGuards results req = pure results

instance runGuardsCons ::
  ( IsSymbol name
  , Row.Cons name guardVal guardsSpec' guardsSpec
  , Row.Cons name (GuardFn guardRes) allGuards' allGuards
  , Row.Cons name guardVal results newResults
  , Row.Lacks name results
  , ToGuardVal guardRes guardVal
  , RunGuards rest guardsSpec allGuards newResults routeGuardSpec
  ) => RunGuards (GCons name rest) guardsSpec allGuards results routeGuardSpec where
  runGuards _ _ allGuards results req = do
    let (guardHandler :: GuardFn guardRes) = Record.get (SProxy :: SProxy name) (to allGuards)
    (guardHandlerResult :: guardRes) <- lift $ guardHandler req
    (guardResult :: guardVal) <- toGuardVal guardHandlerResult
    let newResults = Record.insert (SProxy :: SProxy name) guardResult results
    runGuards (Guards :: _ rest) (GuardTypes :: _ (Record guardsSpec)) allGuards newResults req

class ToGuardVal a b where
  toGuardVal :: a -> Resp.Result b

instance toGuardValEitherStringVal :: ToGuardVal (Either String a) a where
  toGuardVal (Left res) = throwError (Resp.Error $ Resp.internalError (Resp.StringBody res))
  toGuardVal (Right res) = pure res
else instance toGuardValEitherServerErrorVal :: ToGuardVal (Either Resp.Failure a) a where
  toGuardVal (Left err) = throwError err
  toGuardVal (Right res) = pure res
else instance toGuardValIdentity :: ToGuardVal a a where
  toGuardVal = pure

headers :: HTTP.Request -> Aff Headers
headers req = pure (Headers.fromFoldable headersArr)
  where
    headersArr :: Array (Tuple String String)
    headersArr = Object.toUnfoldable $ HTTP.requestHeaders req

rawRequest :: HTTP.Request -> Aff HTTP.Request
rawRequest req = pure req

cookies :: HTTP.Request -> Aff (Map String String)
cookies req = pure (Cookies.requestCookies req)
