module Minefield.Extensions.Nouns.Landmine.Object where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object


-- | States of landmine
data LandmineState
  = LandmineActive
  | LandmineDisarmed

-- | Landmine runtime instance.
data LandmineObject = LandmineObject
  { loObjectInfoRef :: IORef ObjectInfo
  , loPos           :: Pos
  , loPowerRef      :: IORef Int
  , loStateRef      :: IORef LandmineState
  }
