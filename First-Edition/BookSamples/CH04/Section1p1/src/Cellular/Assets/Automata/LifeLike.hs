{-# LANGUAGE DataKinds #-}
module Cellular.Assets.Automata.LifeLike where

import Cellular.Language.Algorithm

type D = 0    -- Dead cell state
type A = 1    -- Alive cell state

type LifeLikeStates =
  '[ 'State "Alive" A
   , 'State "Dead"  D
   ]
