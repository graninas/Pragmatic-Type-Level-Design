{-# LANGUAGE AllowAmbiguousTypes #-}

module TCA3.Automaton where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA3.Types


class Dim2Automaton rule where
  type Cell rule :: *
  emptyCell :: Cell rule
  step :: Dim2Board (Cell rule) -> Dim2Board (Cell rule)

  type DatabaseCell rule :: *
  toDatabaseCell   :: Cell rule -> DatabaseCell rule
  fromDatabaseCell :: DatabaseCell rule -> Cell rule


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
