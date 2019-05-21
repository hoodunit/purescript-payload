module Payload.Guards where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect.Aff (Aff)
import Node.HTTP as HTTP
import Prim.Row as Row
import Prim.RowList (Cons, Nil, kind RowList)
import Record as Record
import Type.Equality (to)
import Type.Row (RLProxy(..))

data Guard (name :: Symbol) a = Guard

type GuardFn a = HTTP.Request -> Aff (Either String a)

class RunGuards
  (routeGuards :: RowList)
  (allGuards :: # Type)
  (results :: # Type)
  (routeGuardSpec :: # Type) | routeGuards allGuards results -> routeGuardSpec where
  runGuards :: RLProxy routeGuards -> Record allGuards -> Record results -> HTTP.Request -> Aff (Either String (Record routeGuardSpec))

instance runGuardsNil :: RunGuards Nil allGuards routeGuardSpec routeGuardSpec where
  runGuards _ allGuards results req = pure (Right results)

instance runGuardsCons ::
  ( IsSymbol name
  , Row.Cons name (HTTP.Request -> Aff (Either String guardVal)) allGuards' allGuards
  , Row.Cons name guardVal results newResults
  , Row.Lacks name results
  , RunGuards rest allGuards newResults routeGuardSpec
  ) => RunGuards (Cons name guardVal rest) allGuards results routeGuardSpec where
  runGuards _ allGuards results req = do
    let guardHandler = Record.get (SProxy :: SProxy name) (to allGuards)
    guardResult <- guardHandler req
    case guardResult of
      Right val -> do
        let newResults = Record.insert (SProxy :: SProxy name) val results
        runGuards (RLProxy :: RLProxy rest) allGuards newResults req
      Left err -> pure (Left err)

request :: GuardFn HTTP.Request
request req = pure (Right req)
