{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Assets.Automata.Replicator where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.LifeLike


-- Replicator (B1357/S1357)
type ReplicatorStep = 'Step ('DefState D)
  '[ 'StateTransition D A ('NeighborsCount A '[1,3,5,7])  -- "Born rule"
   , 'StateTransition A A ('NeighborsCount A '[1,3,5,7])  -- "Survive rule"
   ]

type ReplicatorRule = 'Rule
  @LifeLikeStates
  "Replicator"
  "repl"
  ('AdjacentsLvl 1)
  ReplicatorStep
