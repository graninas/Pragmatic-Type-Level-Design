{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TCA.Arbitrary3S where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA.Types
import TCA.Automaton


newtype Arbitrary3SCell = Arbitrary3SCell Int
  deriving (Show, Eq, Ord, Enum)


instance Dim2Automaton Arbitrary3SCell where
  emptyCell = Arbitrary3SCell 0
  step = step'



toArbitrary3SCell :: Int -> Arbitrary3SCell
toArbitrary3SCell i = Arbitrary3SCell $ i `mod` 3


-- TODO: the actual logic


step' :: Dim2Board Arbitrary3SCell -> Dim2Board Arbitrary3SCell
step' Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
