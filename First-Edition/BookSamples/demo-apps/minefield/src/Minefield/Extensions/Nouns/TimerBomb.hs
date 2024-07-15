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
  (objectType :: Symbol)
  (turns :: Nat)
    -- ^ How much turns before the bomb explodes
type TimerBomb i t = MkObject (TimerBombImpl i "timer-bomb" t)

-- Implementation

instance
  ( KnownSymbol i
  ) =>
  Eval GetIcon (TimerBombImpl i ot t) Char where
  eval _ _ = pure $ head $ symbolVal $ Proxy @i

instance
  ( KnownSymbol i
  , KnownSymbol ot
  ) =>
  Eval GetObjectInfo (TimerBombImpl i ot t) (OType, Char) where
  eval _ _ = do
    let oType = symbolVal $ Proxy @ot
    let icon = head $ symbolVal $ Proxy @i
    pure (oType, icon)

instance
  ( KnownSymbol ot
  ) =>
  Eval GetObjectType (TimerBombImpl i ot t) String where
  eval _ _ = pure $ symbolVal $ Proxy @ot
