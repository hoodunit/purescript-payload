module Payload.Guards where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect.Aff (Aff)
import Node.HTTP as HTTP
import Payload.GuardParsing (GCons, GNil, GuardTypes(..), Guards(..), kind GuardList)
import Prim.Row as Row
import Prim.RowList (Cons, Nil, kind RowList)
import Record as Record
import Type.Equality (to)
import Type.Row (RLProxy(..))

data Guard (name :: Symbol) a = Guard

type GuardFn a = HTTP.Request -> Aff (Either String a)

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
               -> Aff (Either String (Record routeGuardSpec))

instance runGuardsNil :: RunGuards GNil guardsSpec allGuards routeGuardSpec routeGuardSpec where
  runGuards _ _ allGuards results req = pure (Right results)

instance runGuardsCons ::
  ( IsSymbol name
  , Row.Cons name guardVal guardsSpec' guardsSpec
  , Row.Cons name (HTTP.Request -> Aff (Either String guardVal)) allGuards' allGuards
  , Row.Cons name guardVal results newResults
  , Row.Lacks name results
  , RunGuards rest guardsSpec allGuards newResults routeGuardSpec
  ) => RunGuards (GCons name rest) guardsSpec allGuards results routeGuardSpec where
  runGuards _ _ allGuards results req = do
    let guardHandler = Record.get (SProxy :: SProxy name) (to allGuards)
    guardResult <- guardHandler req
    case guardResult of
      Right val -> do
        let newResults = Record.insert (SProxy :: SProxy name) val results
        runGuards (Guards :: _ rest) (GuardTypes :: _ (Record guardsSpec)) allGuards newResults req
      Left err -> pure (Left err)

request :: GuardFn HTTP.Request
request req = pure (Right req)
