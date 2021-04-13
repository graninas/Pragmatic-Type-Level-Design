module HCell.Game.State where

import HCell.Prelude

import HCell.Types
import HCell.Gloss.Types
import HCell.Game.Debug

import qualified Data.Map as Map

data GameState = GameState
  { wndSizeVar        :: TVar GlossWindowSize
  , gridDimsVar       :: TVar GridDimensions
  , bareCellSizeVar   :: TVar BareCellSize
  , cellSpaceSizeVar  :: TVar CellSpaceSize
  , levelVar          :: TVar Level
  , debugOptionsVar   :: TVar DebugOptions
  }
