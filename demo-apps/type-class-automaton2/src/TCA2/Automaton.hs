module TCA2.Automaton where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA2.Types


class Dim2Automaton rule cell where
  emptyCell :: Proxy rule -> cell
  step :: Dim2Board rule cell -> Dim2Board rule cell


initializeBoard
  :: forall rule cell
   . Dim2Automaton rule cell
  => Coords
  -> Map Coords cell
  -> Dim2Board rule cell
initializeBoard (xSize, ySize) srcCells = Dim2Board cells xSize ySize
  where
    cells = V.generate xSize generateX
    generateX x = V.generate ySize (generateY x)
    generateY x y = case Map.lookup (x, y) srcCells of
      Nothing -> emptyCell $ Proxy @rule
      Just cell -> cell
