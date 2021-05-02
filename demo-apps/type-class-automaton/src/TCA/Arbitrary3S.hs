{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TCA.Arbitrary3S where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA.Types
import TCA.Automaton


newtype Arbitary3SCell = Arbitary3SCell Int
  deriving (Show, Eq, Ord, Enum)


instance Dim2Automaton Arbitary3SCell where
  emptyCell = Arbitary3SCell 0
  step = step'



toArbitrary3SCell :: Int -> Arbitary3SCell
toArbitrary3SCell i = Arbitary3SCell $ i `mod` 3


-- TODO: the actual logic


step' :: Dim2Board Arbitary3SCell -> Dim2Board Arbitary3SCell
step' Dim2Board {cells, xSize, ySize} = newBoard
  where
    newCells = cells
    newBoard = Dim2Board newCells xSize ySize
