
module Minefield.Extensions.Nouns.Player where

import CPrelude

import Minefield.Core.Interface
import Minefield.Core.Eval

import Minefield.Extensions.Materialization

import GHC.TypeLits


data PlayerImpl
  (icon :: Symbol)
type Player i = MkObject (PlayerImpl i)

instance
  ( KnownSymbol i
  ) =>
  Eval GetIcon (PlayerImpl i) Char where
  eval _ _ = pure $ head $ symbolVal $ Proxy @i
