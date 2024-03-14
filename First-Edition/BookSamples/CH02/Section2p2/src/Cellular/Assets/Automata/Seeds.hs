{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Cellular.Assets.Automata.Seeds where

import Cellular.Domain.Cell ( Cell(..) )
import Cellular.Domain.Board ( Board, Coords, countAliveNeighbours )
import Cellular.Domain.Automaton ( Automaton(step, code), CellWorld(..) )

import qualified Data.Map as Map


type SeedsRule = "Seeds"       -- DataKinds used here
type Seeds = CellWorld SeedsRule

{-
-- Alternative:

type Seeds = CellWorld "Seeds"
-}

instance Automaton SeedsRule where  -- FlexibleInstances used here
  step :: Seeds -> Seeds            -- InstanceSigs is enabled to show sigs
  step = seedsStep
  code _ = "seeds"


-- B2/S
seedsStep :: Seeds -> Seeds
seedsStep (CW board) = CW board'
  where
    updateCell :: Coords -> Cell
    updateCell pos =
        case (Map.lookup pos board, countAliveNeighbours board pos) of
            (Just Dead, 2)  -> Alive
            _               -> Dead
    board' :: Board
    board' = Map.mapWithKey (\pos _ -> updateCell pos) board
