{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Assets.Automata.GameOfLife where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.LifeLike
import Common.NonEmptyList


-- Game of Life (B3/S23)
type Neighbors3  = 'NeighborsCount A ('List1 3 '[])
type Neighbors23 = 'NeighborsCount A ('List1 2 '[3])

type GoLStep = 'Step ('DefState D)
  '[ 'StateTransition D A Neighbors3
   , 'StateTransition A A Neighbors23
   ]

type GoLRule = 'Rule
  @LifeLikeStates
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  GoLStep

-- Alternative definition of the step with tuples:
type GoLStepTuple =
  '( 'DefState D
   ,  '[ 'StateTransition D A Neighbors3
       , 'StateTransition A A Neighbors23
       ]
   )
