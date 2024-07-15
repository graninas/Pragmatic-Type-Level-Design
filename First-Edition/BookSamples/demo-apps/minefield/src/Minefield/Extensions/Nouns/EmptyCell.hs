module Minefield.Extensions.Nouns.EmptyCell where

import CPrelude

import Minefield.Core.Interface
import Minefield.Core.Eval

import Minefield.Extensions.Materialization

import GHC.TypeLits


data EmptyCellImpl
  (icon :: Symbol)
  (objectType :: Symbol)
type EmptyCell i = MkObject (EmptyCellImpl i "empty-cell")

instance
  ( KnownSymbol i
  ) =>
  Eval GetIcon (EmptyCellImpl i ot) Char where
  eval _ _ = pure $ head $ symbolVal $ Proxy @i

instance
  ( KnownSymbol ot
  ) =>
  Eval GetObjectType (EmptyCellImpl i ot) String where
  eval _ _ = pure $ symbolVal $ Proxy @ot
