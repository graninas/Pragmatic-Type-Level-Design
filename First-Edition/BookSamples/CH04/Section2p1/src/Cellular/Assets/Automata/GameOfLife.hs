{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Assets.Automata.GameOfLife where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.LifeLike


-- Game of Life (B3/S23)
type Neighbors3  = 'NeighborsCount A '[3  ]
type Neighbors23 = 'NeighborsCount A '[2,3]

type GoLStep = 'Step ('DefState D)
  '[ 'StateTransition D A Neighbors3
   , 'StateTransition A A Neighbors23
   ]

type GoLRule = 'Rule
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


neighbors3, neighbors23 :: CellCondition
neighbors3  = NeighborsCount 1 [3  ]
neighbors23 = NeighborsCount 1 [2,3]

goLStep :: CustomStep
goLStep = Step (DefState 0)
  [ StateTransition 0 1 neighbors3
  , StateTransition 1 1 neighbors23
  ]
