module Minefield.Extensions.Nouns.Landmine.Object where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object


-- | States of landmine
data LandmineState
  = LandmineActive
  | LandmineDead
  | LandmineDisarmed
  | LandmineExplosion Int      -- ticks left

-- | Landmine runtime instance.
data LandmineObject = LandmineObject
  { loObjectInfoRef :: IORef ObjectInfo
  , loPos           :: Pos
  , loPower         :: Int
  , loStateRef      :: IORef LandmineState
  }
