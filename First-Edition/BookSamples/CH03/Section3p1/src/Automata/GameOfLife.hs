{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Automata.GameOfLife where

import Cell ( Cell(..) )
import Board ( Board, Coords, countAliveNeighbours )
import Automaton ( Automaton(step, code), CellWorld(..) )

import qualified Data.Map as Map


type GoLRule = "Game of Life"    -- DataKinds used here
type GoL = CellWorld GoLRule

instance Automaton GoLRule where   -- FlexibleInstances used here
  step :: GoL -> GoL               -- InstanceSigs is enabled to show sigs
  step = golStep
  code _ = "gol"


-- B3/S23
golStep :: GoL -> GoL
golStep (CW board) = CW board'
  where
    updateCell :: Coords -> Cell
    updateCell pos =
        case (Map.lookup pos board, countAliveNeighbours board pos) of
            (Just Dead, 3)  -> Alive
            (Just Alive, 2) -> Alive
            (Just Alive, 3) -> Alive
            _               -> Dead
    board' :: Board
    board' = Map.mapWithKey (\pos _ -> updateCell pos) board
