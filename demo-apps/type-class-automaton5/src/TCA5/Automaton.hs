module TCA5.Automaton where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA5.Types


class Dim2Automaton rule cell | rule -> cell where
  emptyCell :: rule -> cell
  step      :: rule -> Dim2Board cell -> Dim2Board cell


initializeBoard
  :: forall rule cell
   . Dim2Automaton rule cell
  => rule
  -> Coords
  -> Map Coords cell
  -> Dim2Board cell
initializeBoard rule (xSize, ySize) srcCells =
  Dim2Board cells xSize ySize
  where
    cells         = V.generate xSize generateX
    generateX x   = V.generate ySize (generateY x)
    generateY x y = case Map.lookup (x, y) srcCells of
      Nothing   -> emptyCell rule
      Just cell -> cell
