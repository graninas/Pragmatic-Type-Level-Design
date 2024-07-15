module Minefield.Extensions.Nouns.EmptyCell where

import CPrelude

import Minefield.Core.Interface
import Minefield.Core.Eval

import Minefield.Extensions.Materialization

import GHC.TypeLits


data EmptyCellImpl
  (icon :: Symbol)
type EmptyCell i = MkObject (EmptyCellImpl i) "empty-cell"

instance
  ( KnownSymbol i
  ) =>
  Eval GetIcon (EmptyCellImpl i) Char where
  eval _ _ = pure $ head $ symbolVal $ Proxy @i
