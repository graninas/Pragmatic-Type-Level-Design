module Minefield.Implementation.SystemNouns.EmptyCell.Object where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object


-- | Empty cell runtime instance.
data EmptyCellObject = EmptyCellObject
  { ecoObjectInfoRef :: IORef ObjectInfo
  , ecoPos           :: Pos
  }
