module HCell.Game.State where

import CPrelude

import HCell.Types
import HCell.Gloss.Types
import HCell.Game.Debug

import qualified Data.Map as Map

data GameState = GameState
  { wndSizeVar        :: TVar GlossWindowSize
  , gridDimsVar       :: TVar GridDimensions
  , bareCellSizeVar   :: TVar BareCellSize
  , cellSpaceSizeVar  :: TVar CellSpaceSize
  , aliveCellsVar     :: TVar AliveCells
  , debugOptionsVar   :: TVar DebugOptions
  }
