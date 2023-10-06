{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Cellular.Assets.Automata.Replicator where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.Boards


-- B1357/S1357
type ReplicatorStep = 'Step
  '[ 'StateTransition 0 1 '[ 'CellsCount 1 '[1,3,5,7]]  -- "Born rule"
   , 'StateTransition 1 1 '[ 'CellsCount 1 '[1,3,5,7]]  -- "Survive rule"
   , 'DefaultTransition 0
   ]

type ReplicatorRule = 'Rule
  "Replicator"
  "repl"
  Open2StateBoard
  ('AdjacentsLvl 1)
  ReplicatorStep
