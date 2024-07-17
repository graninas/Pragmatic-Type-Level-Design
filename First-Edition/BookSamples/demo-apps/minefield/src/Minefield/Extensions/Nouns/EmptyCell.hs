module Minefield.Extensions.Nouns.EmptyCell where

import CPrelude

import TypeLevelDSL.Eval
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
  EvalIO () GetIcon (EmptyCellImpl i ot) Icon where
  evalIO () _ _ = pure $ head $ symbolVal $ Proxy @i

instance
  ( KnownSymbol i
  , KnownSymbol ot
  ) =>
  EvalIO () GetObjectInfo (EmptyCellImpl i ot) ObjectInfo where
  evalIO () _ _ = do
    let oType = symbolVal $ Proxy @ot
    let icon = head $ symbolVal $ Proxy @i
    pure $ ObjectInfo icon (-1, -1) oType True []


instance
  ( KnownSymbol ot
  ) =>
  EvalIO () GetObjectType (EmptyCellImpl i ot) ObjectType where
  evalIO () _ _ = pure $ symbolVal $ Proxy @ot

-- Object

data EmptyCellObject = EmptyCellObject
  { ecoObjectInfoRef :: IORef ObjectInfo
  }
