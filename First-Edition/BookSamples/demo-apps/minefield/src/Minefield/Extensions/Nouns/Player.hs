
module Minefield.Extensions.Nouns.Player where

import CPrelude

import Minefield.Core.Interface
import Minefield.Core.Eval

import Minefield.Extensions.Materialization

import GHC.TypeLits


data PlayerImpl
  (icon :: Symbol)
  (objectType :: Symbol)
type Player i = MkObject (PlayerImpl i "player")

instance
  ( KnownSymbol i
  ) =>
  Eval GetIcon (PlayerImpl i ot) Char where
  eval _ _ = pure $ head $ symbolVal $ Proxy @i

instance
  ( KnownSymbol ot
  ) =>
  Eval GetObjectType (PlayerImpl i ot) String where
  eval _ _ = pure $ symbolVal $ Proxy @ot
