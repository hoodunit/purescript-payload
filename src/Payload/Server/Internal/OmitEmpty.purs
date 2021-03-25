module Payload.Server.Internal.OmitEmpty where

import Data.Symbol (class IsSymbol)
import Payload.Internal.Route (Undefined)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Type.Proxy (Proxy(..))

class OmitEmpty (rows :: Row Type) (rowsNoEmpty :: Row Type) | rows -> rowsNoEmpty where
  omitEmpty :: Record rows -> Record rowsNoEmpty

instance omitEmptyAny ::
  ( RowToList rows rl
  , OmitEmptyFields rl rows rowsNoEmpty
  ) => OmitEmpty rows rowsNoEmpty where
  omitEmpty record = omitEmptyFields (Proxy :: _ rl) record

class OmitEmptyFields
      (rl :: RowList Type)
      (rows :: Row Type)
      (rowsNoEmpty :: Row Type)
      | rl rows -> rowsNoEmpty where
  omitEmptyFields :: Proxy rl -> Record rows -> Record rowsNoEmpty

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
    omitEmptyFields (Proxy :: _ rest) newRecord
    where
      -- Remove field if empty
      newRecord :: Record nextRows
      newRecord = removeEmptyField (Proxy :: _ label) (Proxy :: _ fieldRl) record
else instance omitEmptyFieldsConsUndefined ::
  ( Row.Cons label Undefined nextRows rows
  , Row.Lacks label nextRows
  , IsSymbol label
  , OmitEmptyFields rest nextRows rowsNoEmpty
  ) => OmitEmptyFields
        (RowList.Cons label Undefined rest)
        rows
        rowsNoEmpty where
  omitEmptyFields _ record = omitEmptyFields (Proxy :: _ rest) newRecord
    where
      newRecord :: Record nextRows
      newRecord = Record.delete (Proxy :: _ label) record
else instance omitEmptyFieldsConsOther ::
  ( OmitEmptyFields rest rows rowsNoEmpty
  ) => OmitEmptyFields
        (RowList.Cons label fieldType rest)
        rows
        rowsNoEmpty where
  omitEmptyFields _ record = omitEmptyFields (Proxy :: _ rest) record

class RemoveEmptyField
      (label :: Symbol)
      (fieldRl :: RowList Type)
      (rows :: Row Type)
      (rowsNoEmpty :: Row Type)
      | fieldRl rows -> rowsNoEmpty where
  removeEmptyField :: Proxy label -> Proxy fieldRl -> Record rows -> Record rowsNoEmpty

instance removeEmptyFieldNil ::
  ( Row.Cons label fieldType rowsNoEmpty rows
  , IsSymbol label
  , Row.Lacks label rowsNoEmpty
  ) => RemoveEmptyField label RowList.Nil rows rowsNoEmpty where
  removeEmptyField _ _ record = Record.delete (Proxy :: _ label) record
else instance removeEmptyFieldNotEmptyUndefined ::
  ( Row.Cons label fieldType rowsNoEmpty rows
  , IsSymbol label
  , Row.Lacks label rowsNoEmpty
  ) => RemoveEmptyField label (RowList.Cons key Undefined rl) rows rowsNoEmpty where
  removeEmptyField _ _ record = Record.delete (Proxy :: _ label) record
else instance removeEmptyFieldNotEmptyOther ::
  RemoveEmptyField label (RowList.Cons key valType rl) rowsNoEmpty rowsNoEmpty where
  removeEmptyField _ _ record = record
