module TCA3.Arbitrary2S where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA3.Types
import TCA3.Automaton

data Arbitrary2S

instance Dim2Automaton Arbitrary2S TwoStateCell where
  emptyCell = Dead
  step = golStep





-- TODO: the actual logic


golStep :: Dim2Board Arbitrary2S TwoStateCell -> Dim2Board Arbitrary2S TwoStateCell
golStep Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
