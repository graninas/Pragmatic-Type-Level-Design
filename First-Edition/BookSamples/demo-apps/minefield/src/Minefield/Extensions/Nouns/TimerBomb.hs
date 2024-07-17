module Minefield.Extensions.Nouns.TimerBomb where

import CPrelude

import TypeLevelDSL.Eval
import Minefield.Core.Types
import Minefield.Core.Interface
import Minefield.Core.Object

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
  EvalIO () GetIcon (TimerBombImpl i ot t) Icon where
  evalIO () _ _ = pure $ head $ symbolVal $ Proxy @i

instance
  ( KnownSymbol i
  , KnownSymbol ot
  ) =>
  EvalIO () GetObjectInfo (TimerBombImpl i ot t) ObjectInfo where
  evalIO () _ _ = do
    let oType = symbolVal $ Proxy @ot
    let icon = head $ symbolVal $ Proxy @i
    pure $ ObjectInfo icon (-1, -1) oType True []

instance
  ( KnownSymbol ot
  ) =>
  EvalIO () GetObjectType (TimerBombImpl i ot t) ObjectType where
  evalIO () _ _ = pure $ symbolVal $ Proxy @ot

-- Object

data TimerBombObject = TimerBombObject
  { tboObjectInfoRef :: IORef ObjectInfo
  , tboTurnsRef      :: IORef Int
  }
