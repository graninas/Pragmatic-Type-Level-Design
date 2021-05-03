{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TCA2.Arbitrary3S where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA2.Types
import TCA2.Automaton

data Arbitrary3S

newtype Arbitrary3SCell = Arbitrary3SCell Int
  deriving (Show, Eq, Ord, Enum)



instance Dim2Automaton Arbitrary3S Arbitrary3SCell where
  emptyCell _ = Arbitrary3SCell 0
  step = step'



toArbitrary3SCell :: Int -> Arbitrary3SCell
toArbitrary3SCell i = Arbitrary3SCell $ i `mod` 3


-- TODO: the actual logic


step' :: Dim2Board Arbitrary3S Arbitrary3SCell -> Dim2Board Arbitrary3S Arbitrary3SCell
step' Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
