module Minefield.Extensions.Nouns.EmptyCell where

import CPrelude

import Minefield.Core.Language
import Minefield.Core.Eval

import Minefield.Extensions.Materialization

import GHC.TypeLits


data EmptyCellImpl
  (icon :: Symbol)
type EmptyCell i = MkObject (EmptyCellImpl i)

instance
  ( KnownSymbol i
  ) =>
  Eval GetIcon (EmptyCellImpl i) Char where
  eval _ _ = head $ symbolVal $ Proxy @i
