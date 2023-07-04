{-# LANGUAGE AllowAmbiguousTypes #-}

module TCA4.Automaton where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA4.Types


class Dim2Automaton rule where
  data Cell rule :: *
  emptyCell :: Cell rule
  step :: Dim2Board (Cell rule) -> Dim2Board (Cell rule)


initializeBoard
  :: forall rule
   . Dim2Automaton rule
  => Coords
  -> Map Coords (Cell rule)
  -> Dim2Board (Cell rule)
initializeBoard (xSize, ySize) srcCells = Dim2Board cells xSize ySize
  where
    cells = V.generate xSize generateX
    generateX x = V.generate ySize (generateY x)
    generateY x y = case Map.lookup (x, y) srcCells of
      Nothing -> emptyCell @rule
      Just cell -> cell
