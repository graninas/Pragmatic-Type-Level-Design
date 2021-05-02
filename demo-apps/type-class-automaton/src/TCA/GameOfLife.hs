module TCA.GameOfLife where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA.Types
import TCA.Automaton


data GoLCell
  = GoLAlive
  | GoLDead
  deriving (Show, Eq, Ord, Enum)


instance Dim2Automaton GoLCell where
  emptyCell = GoLDead
  step = golStep





-- TODO: the actual logic


golStep :: Dim2Board GoLCell -> Dim2Board GoLCell
golStep Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
