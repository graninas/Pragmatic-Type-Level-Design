{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Assets.Automata.GameOfLife where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.Boards
import Cellular.Assets.Automata.LifeLike
import Common.NonEmptyList


-- Game of Life (B3/S23)
type GoLStep = 'Step ('DefState D)
  '[ 'StateTransition D A ('NeighborsCount A ('List1 3 '[]))
   , 'StateTransition A A ('NeighborsCount A ('List1 2 '[3]))
   ]

type GoLRule = 'Rule
  @LifeLikeStates
  "Game of Life"
  "gol"
  OpenBoard
  ('AdjacentsLvl 1)
  GoLStep
