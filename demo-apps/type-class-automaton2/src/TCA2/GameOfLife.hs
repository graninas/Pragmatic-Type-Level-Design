module TCA2.GameOfLife where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA2.Types
import TCA2.Automaton

data GoLRule

instance Dim2Automaton GoLRule TwoStateCell where
  emptyCell _ = Dead
  step = golStep





-- TODO: the actual logic


golStep :: Dim2Board GoLRule TwoStateCell -> Dim2Board GoLRule TwoStateCell
golStep Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
