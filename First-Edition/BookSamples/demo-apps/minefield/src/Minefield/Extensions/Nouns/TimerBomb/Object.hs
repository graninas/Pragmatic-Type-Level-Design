module Minefield.Extensions.Nouns.TimerBomb.Object where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object


-- | Timer bomb runtime instance.

data TimerBombState
  = TimerBombTicking Int        -- ticks left
  | TimerBombExplosion Int      -- ticks left
  | TimerBombDead

data TimerBombObject = TimerBombObject
  { tboObjectInfoRef :: IORef ObjectInfo
  , tboPos           :: Pos
  , tboStateRef      :: IORef TimerBombState
  }

