{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Assets.Automata.Replicator where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.Boards
import Cellular.Assets.Automata.LifeLike
import Common.NonEmptyList


type ReplNs = 'List1 1 '[3,5,7]

-- Replicator (B1357/S1357)
type ReplicatorStep = 'Step ('DefState D)
  '[ 'StateTransition D A ('NeighborsCount A ReplNs)
   , 'StateTransition A A ('NeighborsCount A ReplNs)
   ]

type ReplicatorRule = 'Rule
  @LifeLikeStates
  "Replicator"
  "repl"
  OpenBoard
  ('AdjacentsLvl 1)
  ReplicatorStep
