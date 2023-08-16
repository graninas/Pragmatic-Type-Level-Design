{-# LANGUAGE DataKinds #-}
module GameOfLife where

import Automaton ( CellWorld )

import qualified Data.Map as Map


type GoLRule = "Game of Life"    -- DataKinds used here
type GoL = CellWorld GoLRule

{-
-- Alternative:

type GoL = CellWorld "Game of Life"
-}

-- TODO: rules

golStep :: GoL -> GoL
golStep = error "Not implemented"

