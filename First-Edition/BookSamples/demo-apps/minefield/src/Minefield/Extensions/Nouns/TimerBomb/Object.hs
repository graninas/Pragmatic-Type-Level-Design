module Minefield.Extensions.Nouns.TimerBomb.Object where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object


-- | Timer bomb runtime instance.

data TimerBombState
  = TimerBombTicking Int (IORef OverhaulIconBatch)
      -- ^ int == ticks left
  | TimerBombExplosion Int
      -- ^ int == ticks left
  | TimerBombDead
  | TimerBombDisarmed

data TimerBombObject = TimerBombObject
  { tboObjectInfoRef :: IORef ObjectInfo
  , tboPos           :: Pos
  , tboStateRef      :: IORef TimerBombState
  }

