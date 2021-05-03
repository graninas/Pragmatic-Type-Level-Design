module TCA3.GameOfLife where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA3.Types
import TCA3.Automaton

data GoLRule


-- data GoLCell = GoLAlive | GoLDead
--   deriving (Show, Eq, Ord, Enum)

instance Dim2Automaton GoLRule TwoStateCell where
  emptyCell = GoLDead
  step = golStep





-- TODO: the actual logic


golStep :: Dim2Board GoLRule TwoStateCell -> Dim2Board GoLRule TwoStateCell
golStep Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
