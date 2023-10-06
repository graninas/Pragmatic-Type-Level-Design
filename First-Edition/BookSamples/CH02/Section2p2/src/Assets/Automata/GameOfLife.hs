{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Assets.Automata.GameOfLife where

import Domain.Cell ( Cell(..) )
import Domain.Board ( Board, Coords, countAliveNeighbours )
import Domain.Automaton ( Automaton(step, code), CellWorld(..) )

import qualified Data.Map as Map


type GoLRule = "Game of Life"    -- DataKinds used here
type GoL = CellWorld GoLRule

{-
-- Alternative:

type GoL = CellWorld "Game of Life"
-}

instance Automaton GoLRule where   -- FlexibleInstances used here
  step :: GoL -> GoL               -- InstanceSigs is enabled to show sigs
  step = golStep
  code _ = "gol"

-- B3/S23
golStep :: GoL -> GoL
golStep (CW board) = CW board'
  where
    updateCell :: Coords -> Cell -> Cell
    updateCell pos cell =
        case (cell, countAliveNeighbours board pos) of
            (Dead,  3) -> Alive
            (Alive, 2) -> Alive
            (Alive, 3) -> Alive
            _               -> Dead
    board' :: Board
    board' = Map.mapWithKey updateCell board
