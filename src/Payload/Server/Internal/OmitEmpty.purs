module Payload.Server.Internal.OmitEmpty where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Payload.Internal.Route (Undefined)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
import Record as Record
import Type.RowList (RLProxy(..))

class OmitEmpty (rows :: # Type) (rowsNoEmpty :: # Type) | rows -> rowsNoEmpty where
  omitEmpty :: Record rows -> Record rowsNoEmpty

instance omitEmptyAny ::
  ( RowToList rows rl
  , OmitEmptyFields rl rows rowsNoEmpty
  ) => OmitEmpty rows rowsNoEmpty where
  omitEmpty record = omitEmptyFields (RLProxy :: _ rl) record

class OmitEmptyFields
      (rl :: RowList)
      (rows :: # Type)
      (rowsNoEmpty :: # Type)
      | rl rows -> rowsNoEmpty where
  omitEmptyFields :: RLProxy rl -> Record rows -> Record rowsNoEmpty

instance omitEmptyFieldsNil :: OmitEmptyFields RowList.Nil rowsNoEmpty rowsNoEmpty where
  omitEmptyFields _ record = record
else instance omitEmptyFieldsConsRecord ::
  ( RowToList fieldRows fieldRl
  , Row.Cons label (Record fieldRows) rowsRest rows
  , RemoveEmptyField label fieldRl rows nextRows
  , OmitEmptyFields rest nextRows rowsNoEmpty
  ) => OmitEmptyFields
        (RowList.Cons label (Record fieldRows) rest)
        rows
        rowsNoEmpty where
  omitEmptyFields _ record =
    -- Iterate through rest with maybe one less field
    omitEmptyFields (RLProxy :: _ rest) newRecord
    where
      -- Remove field if empty
      newRecord :: Record nextRows
      newRecord = removeEmptyField (SProxy :: _ label) (RLProxy :: _ fieldRl) record
else instance omitEmptyFieldsConsUndefined ::
  ( Row.Cons label Undefined nextRows rows
  , Row.Lacks label nextRows
  , IsSymbol label
  , OmitEmptyFields rest nextRows rowsNoEmpty
  ) => OmitEmptyFields
        (RowList.Cons label Undefined rest)
        rows
        rowsNoEmpty where
  omitEmptyFields _ record = omitEmptyFields (RLProxy :: _ rest) newRecord
    where
      newRecord :: Record nextRows
      newRecord = Record.delete (SProxy :: _ label) record
else instance omitEmptyFieldsConsOther ::
  ( OmitEmptyFields rest rows rowsNoEmpty
  ) => OmitEmptyFields
        (RowList.Cons label fieldType rest)
        rows
        rowsNoEmpty where
  omitEmptyFields _ record = omitEmptyFields (RLProxy :: _ rest) record

class RemoveEmptyField
      (label :: Symbol)
      (fieldRl :: RowList)
      (rows :: # Type)
      (rowsNoEmpty :: # Type)
      | fieldRl rows -> rowsNoEmpty where
  removeEmptyField :: SProxy label -> RLProxy fieldRl -> Record rows -> Record rowsNoEmpty

instance removeEmptyFieldNil ::
  ( Row.Cons label fieldType rowsNoEmpty rows
  , IsSymbol label
  , Row.Lacks label rowsNoEmpty
  ) => RemoveEmptyField label RowList.Nil rows rowsNoEmpty where
  removeEmptyField _ _ record = Record.delete (SProxy :: _ label) record
else instance removeEmptyFieldNotEmptyUndefined ::
  ( Row.Cons label fieldType rowsNoEmpty rows
  , IsSymbol label
  , Row.Lacks label rowsNoEmpty
  ) => RemoveEmptyField label (RowList.Cons key Undefined rl) rows rowsNoEmpty where
  removeEmptyField _ _ record = Record.delete (SProxy :: _ label) record
else instance removeEmptyFieldNotEmptyOther ::
  RemoveEmptyField label (RowList.Cons key valType rl) rowsNoEmpty rowsNoEmpty where
  removeEmptyField _ _ record = record
