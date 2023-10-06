{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Cellular.Assets.Automata.GameOfLife where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.Boards


-- B3/S23
type GoLStep = 'Step
  '[ 'StateTransition 0 1 '[ 'CellsCount 1 '[3 ]]   -- "Born rule"
   , 'StateTransition 1 1 '[ 'CellsCount 1 '[2,3]]  -- "Survive rule"
   , 'DefaultTransition 0
   ]

type GoLRule = 'Rule
  "Game of Life"
  "gol"
  Open2StateBoard
  ('AdjacentsLvl 1)
  GoLStep
