{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TCA5.Arbitrary3S where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA5.Types
import TCA5.Automaton

data A3S = A3S
data A3SCell = A3SC0 | A3SC1 | A3SC2
  deriving (Show, Eq, Ord, Enum)



instance Dim2Automaton A3S A3SCell where
  emptyCell _ = A3SC0
  evolve rule = rule
  step _ = step'



toArbitrary3SCell :: Int -> A3SCell
toArbitrary3SCell 1 = A3SC1
toArbitrary3SCell 2 = A3SC2
toArbitrary3SCell _ = A3SC0


-- TODO: the actual logic


step' :: Dim2Board A3SCell -> Dim2Board A3SCell
step' Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
