{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Cellular.Assets.Automata.Replicator where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.Boards
import Cellular.Assets.Automata.LifeLike


-- B1357/S1357
type ReplicatorStep = 'Step
  '[ 'StateTransition D A '[ 'CellsCount A '[1,3,5,7]]  -- "Born rule"
   , 'StateTransition A A '[ 'CellsCount A '[1,3,5,7]]  -- "Survive rule"
   , 'DefaultTransition D
   ]

type ReplicatorRule = 'Rule
  "Replicator"
  "repl"
  Open2StateBoard
  ('AdjacentsLvl 1)
  ReplicatorStep
