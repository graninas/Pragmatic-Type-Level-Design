module TCA2.Arbitrary2S where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA2.Types
import TCA2.Automaton

data Arbitrary2S

instance Dim2Automaton Arbitrary2S TwoStateCell where
  emptyCell _ = Dead
  step = step'





-- TODO: the actual logic


step' :: Dim2Board Arbitrary2S TwoStateCell -> Dim2Board Arbitrary2S TwoStateCell
step' Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
