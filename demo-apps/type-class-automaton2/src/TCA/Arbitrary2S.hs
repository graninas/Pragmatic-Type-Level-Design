module TCA.Arbitrary2S where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA.Types
import TCA.Automaton

data Arbitrary2S

instance Dim2Automaton Arbitrary2S TwoStateCell where
  emptyCell _ = Dead
  step = golStep





-- TODO: the actual logic


golStep :: Dim2Board Arbitrary2S TwoStateCell -> Dim2Board Arbitrary2S TwoStateCell
golStep Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
