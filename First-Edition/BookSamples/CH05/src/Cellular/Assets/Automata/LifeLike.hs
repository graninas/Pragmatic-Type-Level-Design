{-# LANGUAGE DataKinds #-}
module Cellular.Assets.Automata.LifeLike where

import Cellular.Language.Algorithm


type A = 1
type D = 0

type Alive = 'State "Alive" A
type Dead  = 'State "Dead"  D

type LifeLikeStates =
  '[ Alive
   , Dead
   ]

