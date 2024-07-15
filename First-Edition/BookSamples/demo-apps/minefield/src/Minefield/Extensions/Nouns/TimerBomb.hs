module Minefield.Extensions.Nouns.TimerBomb where

import CPrelude

import Minefield.Core.Interface
import Minefield.Core.Eval

import Minefield.Extensions.Materialization

import GHC.TypeLits


-- | Timer bomb.
--   Power is always 2.
data TimerBombImpl
  (icon :: Symbol)
  (turns :: Nat)
    -- ^ How much turns before the bomb explodes
type TimerBomb i t = MkObject (TimerBombImpl i t)

instance
  ( KnownSymbol i
  ) =>
  Eval GetIcon (TimerBombImpl i t) Char where
  eval _ _ = pure $ head $ symbolVal $ Proxy @i
