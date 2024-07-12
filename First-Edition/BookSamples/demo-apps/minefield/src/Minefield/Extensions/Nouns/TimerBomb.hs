module Minefield.Extensions.Nouns.TimerBomb where

import CPrelude

import Minefield.Core.Language
import Minefield.Core.System
import Minefield.Core.Eval

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
  eval _ _ = head $ symbolVal $ Proxy @i
