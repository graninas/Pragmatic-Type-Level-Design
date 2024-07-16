module Minefield.Extensions.Nouns.EmptyCell where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Types
import Minefield.Core.Interface
import Minefield.Core.Object

import Minefield.Extensions.Materialization

import GHC.TypeLits


data EmptyCellImpl
  (icon :: Symbol)
  (objectType :: Symbol)
type EmptyCell i = MkObject (EmptyCellImpl i "empty-cell")

-- Implementations

instance
  ( KnownSymbol i
  ) =>
  Eval () GetIcon (EmptyCellImpl i ot) Icon where
  eval () _ _ = pure $ head $ symbolVal $ Proxy @i

instance
  ( KnownSymbol i
  , KnownSymbol ot
  ) =>
  Eval () GetObjectInfo (EmptyCellImpl i ot) ObjectInfo where
  eval () _ _ = do
    let oType = symbolVal $ Proxy @ot
    let icon = head $ symbolVal $ Proxy @i
    pure $ ObjectInfo icon (-1, -1) oType True []


instance
  ( KnownSymbol ot
  ) =>
  Eval () GetObjectType (EmptyCellImpl i ot) ObjectType where
  eval () _ _ = pure $ symbolVal $ Proxy @ot

-- Object

data EmptyCellObject = EmptyCellObject
  { ecoObjectInfoRef :: IORef ObjectInfo
  }
