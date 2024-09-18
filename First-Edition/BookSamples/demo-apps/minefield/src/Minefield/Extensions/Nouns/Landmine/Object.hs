module Minefield.Extensions.Nouns.Landmine.Object where

import CPrelude

import Minefield.Core.Object


-- | Landmine runtime instance.
data LandmineObject = LandmineObject
  { loObjectInfoRef :: IORef ObjectInfo
  , loPowerRef      :: IORef Int
  }
