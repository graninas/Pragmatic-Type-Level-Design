{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TCA4.Arbitrary3S where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA4.Types
import TCA4.Automaton

data Arbitrary3S


instance Dim2Automaton Arbitrary3S where

  data Cell Arbitrary3S = A3SState1 | A3SState2 | A3SState3
    deriving (Show, Eq, Ord, Enum)

  emptyCell = A3SState1
  step = step'



toArbitrary3SCell :: Int -> Cell Arbitrary3S
toArbitrary3SCell i = case i `mod` 3 of
  0 -> A3SState1
  1 -> A3SState2
  _ -> A3SState3


-- TODO: the actual logic


step' :: Dim2Board (Cell Arbitrary3S) -> Dim2Board (Cell Arbitrary3S)
step' Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
