module TCA4.GameOfLife where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA4.Types
import TCA4.Automaton

data GoLRule


instance Dim2Automaton GoLRule where

  data Cell GoLRule = GoLAlive | GoLDead
    deriving (Show, Eq, Ord, Enum)

  emptyCell = GoLDead
  step = golStep





-- TODO: the actual logic


golStep :: Dim2Board (Cell GoLRule) -> Dim2Board (Cell GoLRule)
golStep Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
