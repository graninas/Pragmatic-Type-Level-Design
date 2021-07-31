module TCA5.Arbitrary2S where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA5.Types
import TCA5.Automaton

data A2S = A2S
data A2SCell = A2SAlive | A2SDead
  deriving (Show, Eq, Ord, Enum)

instance Dim2Automaton A2S A2SCell where
  emptyCell _ = A2SDead
  evolve rule = rule
  step _ = step'




-- TODO: the actual logic


step' :: Dim2Board A2SCell -> Dim2Board A2SCell
step' Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
