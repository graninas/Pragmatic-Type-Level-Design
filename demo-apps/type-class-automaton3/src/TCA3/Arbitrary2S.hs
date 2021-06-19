module TCA3.Arbitrary2S where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA3.Types
import TCA3.Automaton

data Arbitrary2S

instance Dim2Automaton Arbitrary2S where
  type Cell Arbitrary2S = TwoStateCell
  emptyCell = Dead
  step = step'





-- TODO: the actual logic


step' :: Dim2Board (Cell Arbitrary2S) -> Dim2Board (Cell Arbitrary2S)
step' Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
