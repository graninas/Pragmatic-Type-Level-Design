module TCA.Automaton where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA.Types


class Dim2Automaton cell where
  emptyCell :: cell
  step :: Dim2Board cell -> Dim2Board cell


initialize
  :: forall cell
   . Dim2Automaton cell
  => Coords
  -> Map Coords cell
  -> Dim2Board cell
initialize (xSize, ySize) srcCells = Dim2Board cells xSize ySize
  where
    cells = V.generate xSize generateX
    generateX x = V.generate ySize (generateY x)
    generateY x y = case Map.lookup (x, y) srcCells of
      Nothing -> emptyCell
      Just cell -> cell
