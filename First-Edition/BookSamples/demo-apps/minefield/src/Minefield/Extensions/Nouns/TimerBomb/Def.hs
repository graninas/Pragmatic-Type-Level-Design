module Minefield.Extensions.Nouns.TimerBomb.Def where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits


-- | Timer bomb definition.
--   Power is always 2.
--   Allowed turns: [1..9]
data TimerBombDef
  (icon :: Symbol)
  (objectType :: Symbol)
  (turns :: Nat)
    -- ^ How much turns before the bomb explodes
type TimerBomb i t = MkObjectTemplate (TimerBombDef i "timer-bomb" t)
