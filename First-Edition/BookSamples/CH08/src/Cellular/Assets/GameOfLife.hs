{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module Cellular.Assets.GameOfLife where

import Cellular.Language.Automaton
import Cellular.Implementation.Automaton

import GHC.TypeLits


type A = State "Alive" 1
type D = State "Dead" 0

type Neighbors3  = NeighborsCount A '[3  ]
type Neighbors23 = NeighborsCount A '[2,3]

type GoLStep = Step ('DefState D)
  '[ 'StateTransition D A Neighbors3
   , 'StateTransition A A Neighbors23
   ]

type GoLRule = Rule
  "Game of Life"
  "gol"
  (AdjacentsLvl 1)
  GoLStep
