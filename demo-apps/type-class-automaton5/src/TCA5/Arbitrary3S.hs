{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TCA5.Arbitrary3S where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA5.Types
import TCA5.Automaton

data Arbitrary3S
data Arbitrary3SCell = A3S0 | A3S1 | A3S2
  deriving (Show, Eq, Ord, Enum)



instance Dim2Automaton Arbitrary3S Arbitrary3SCell where
  emptyCell = A3S0
  step = step'



toArbitrary3SCell :: Int -> Arbitrary3SCell
toArbitrary3SCell 1 = A3S1
toArbitrary3SCell 2 = A3S2
toArbitrary3SCell _ = A3S0


-- TODO: the actual logic


step' :: Dim2Board Arbitrary3SCell -> Dim2Board Arbitrary3SCell
step' Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
