module TCA3.GameOfLife where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA3.Types
import TCA3.Automaton

data GoLRule


data DBCell = DBCell Int

instance Dim2Automaton GoLRule where
  type Cell GoLRule = TwoStateCell
  emptyCell = Dead
  step = golStep

  type DatabaseCell GoLRule = DBCell
  toDatabaseCell Dead = DBCell 0
  toDatabaseCell Alive = DBCell 1
  fromDatabaseCell (DBCell 0) = Dead
  fromDatabaseCell (DBCell 1) = Alive


getGoLAliveCell :: Cell GoLRule
getGoLAliveCell = Alive

getGoLAliveCellSame :: TwoStateCell
getGoLAliveCellSame = Alive



-- TODO: the actual logic

golStep :: Dim2Board (Cell GoLRule) -> Dim2Board (Cell GoLRule)
golStep Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize

golStepSame :: Dim2Board TwoStateCell -> Dim2Board TwoStateCell
golStepSame = golStep
