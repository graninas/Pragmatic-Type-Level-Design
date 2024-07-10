{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module Cellular.Assets.ColoredLife where

import Cellular.Language.Automaton
import Cellular.Implementation.Automaton
import Cellular.Assets.GameOfLife

import GHC.TypeLits


data ColorType = Red | Green | Blue

data ColoredStateImpl
  (color :: ColorType)
type ColoredState c = MkState (ColoredStateImpl c)

type R = ColoredState 'Red
type G = ColoredState 'Blue
type B = ColoredState 'Green

type Neighbors2 st = NeighborsCount st '[2]

type ColoredStep = Step ('DefState D)
  '[ 'StateTransition D A Neighbors3
   , 'StateTransition A R (Neighbors2 A)
   , 'StateTransition R G (Neighbors2 R)
   , 'StateTransition G B (Neighbors2 G)
   ]

type ColoredRule = Rule
  "Colored Life"
  "col"
  (AdjacentsLvl 1)
  ColoredStep


data States (sts :: [IState])
type MyStates = '[A, D, R, G, B]
