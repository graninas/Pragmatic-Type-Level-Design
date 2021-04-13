module HCell.Types where

import HCell.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

type Coords = (Int, Int)

newtype CellIdxs = CellIdxs (Int, Int)
  deriving (Eq, Ord, Show)

newtype GridDimensions  = GridDimensions CellIdxs
newtype GridCellSize    = GridCellSize Int
newtype BareCellSize    = BareCellSize Int
newtype BareCellHalf    = BareCellHalf Int
newtype BaseShift       = BaseShift Coords
newtype CellSpaceSize   = CellSpaceSize Int

type Level = Set.Set Coords
