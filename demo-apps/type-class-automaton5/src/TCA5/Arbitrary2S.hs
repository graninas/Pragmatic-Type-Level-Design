module TCA5.Arbitrary2S where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA5.Types
import TCA5.Automaton

data Arbitrary2S
data Arbitrary2SCell = A2SAlive | A2SDead
  deriving (Show, Eq, Ord, Enum)

instance Dim2Automaton Arbitrary2S Arbitrary2SCell where
  emptyCell = A2SDead
  step = some2D2SAutomatonStep





-- TODO: the actual logic


some2D2SAutomatonStep :: Dim2Board Arbitrary2SCell -> Dim2Board Arbitrary2SCell
some2D2SAutomatonStep Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
