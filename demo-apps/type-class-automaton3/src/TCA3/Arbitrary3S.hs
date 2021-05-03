{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TCA3.Arbitrary3S where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA3.Types
import TCA3.Automaton

data Arbitrary3S

newtype Arbitrary3SCell = Arbitrary3SCell Int
  deriving (Show, Eq, Ord, Enum)



instance Dim2Automaton Arbitrary3S where
  type Cell Arbitrary3S = Arbitrary3SCell
  emptyCell = Arbitrary3SCell 0
  step = step'



toArbitrary3SCell :: Int -> Arbitrary3SCell
toArbitrary3SCell i = Arbitrary3SCell $ i `mod` 3


-- TODO: the actual logic


step' :: Dim2Board (Cell Arbitrary3S) -> Dim2Board (Cell Arbitrary3S)
step' Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
