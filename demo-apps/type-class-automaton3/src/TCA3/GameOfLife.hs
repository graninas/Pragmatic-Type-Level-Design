module TCA3.GameOfLife where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA3.Types
import TCA3.Automaton

data GoLRule


instance Dim2Automaton GoLRule where
  type Cell GoLRule = TwoStateCell
  emptyCell = Dead
  step = golStep





-- TODO: the actual logic


golStep :: Dim2Board (Cell GoLRule) -> Dim2Board (Cell GoLRule)
golStep Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
