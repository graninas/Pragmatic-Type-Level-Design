{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module GameOfLife where

import Board ( loadBoardFromFile, saveBoardToFile, Board )
import Automaton ( Automaton(..), CellWorld )

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

-- TODO: rules

golStep :: GoL -> GoL
golStep = error "Not implemented"

