module TCA4.Arbitrary2S where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA4.Types
import TCA4.Automaton

data Arbitrary2S

instance Dim2Automaton Arbitrary2S where

  data Cell Arbitrary2S = A2SState1 | A2SState2
    deriving (Show, Eq, Ord, Enum)

  emptyCell = A2SState1
  step = step'





-- TODO: the actual logic


step' :: Dim2Board (Cell Arbitrary2S) -> Dim2Board (Cell Arbitrary2S)
step' Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
