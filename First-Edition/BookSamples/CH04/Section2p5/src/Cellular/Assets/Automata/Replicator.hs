{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Assets.Automata.Replicator where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.LifeLike
import Common.NonEmptyList


-- Replicator (B1357/S1357)
type Neighbors1357 = 'NeighborsCount A ('List1 1 '[3,5,7])

type ReplicatorStep = 'Step ('DefState D)
  '[ 'StateTransition D A Neighbors1357  -- "Born rule"
   , 'StateTransition A A Neighbors1357  -- "Survive rule"
   ]

type ReplicatorRule = 'Rule
  @LifeLikeStates
  "Replicator"
  "repl"
  ('AdjacentsLvl 1)
  ReplicatorStep
