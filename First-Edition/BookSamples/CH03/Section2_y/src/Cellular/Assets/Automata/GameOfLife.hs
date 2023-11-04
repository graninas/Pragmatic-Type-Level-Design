{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Cellular.Assets.Automata.GameOfLife where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.Boards
import Cellular.Assets.Automata.LifeLike


-- Game of Life rule (B3/S23):
type GoLStep = 'Step
  '[ 'StateTransition D A '[ 'CellsCount A '[3  ]]
   , 'StateTransition A A '[ 'CellsCount A '[2,3]]
   , 'DefaultTransition D
   ]

type GoLRule = 'Rule
  "Game of Life"
  "gol"
  Open2StateBoard
  ('AdjacentsLvl 1)
  GoLStep
