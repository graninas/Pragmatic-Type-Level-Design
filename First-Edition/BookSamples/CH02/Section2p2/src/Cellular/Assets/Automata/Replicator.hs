{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Cellular.Assets.Automata.Replicator where

import Cellular.Domain.Cell ( Cell(..) )
import Cellular.Domain.Board ( Board, Coords, countAliveNeighbours )
import Cellular.Domain.Automaton ( Automaton(step, code), CellWorld(..) )

import qualified Data.Map as Map

type ReplicatorRule = "Replicator"       -- DataKinds used here
type Replicator = CellWorld ReplicatorRule

{-
-- Alternative:

type Replicator = CellWorld "Replicator"
-}

instance Automaton ReplicatorRule where  -- FlexibleInstances used here
  step :: Replicator -> Replicator       -- InstanceSigs is enabled to show sigs
  step = replicatorStep
  code _ = "repl"


-- B1357/S1357
replicatorStep :: Replicator -> Replicator
replicatorStep (CW board) = CW board'
  where
    updateCell :: Coords -> Cell
    updateCell pos =
        case (Map.lookup pos board, countAliveNeighbours board pos) of
            (Just Dead, 1)  -> Alive
            (Just Dead, 3)  -> Alive
            (Just Dead, 5)  -> Alive
            (Just Dead, 7)  -> Alive
            (Just Alive, 1) -> Alive
            (Just Alive, 3) -> Alive
            (Just Alive, 5) -> Alive
            (Just Alive, 7) -> Alive
            _               -> Dead
    board' :: Board
    board' = Map.mapWithKey (\pos _ -> updateCell pos) board
