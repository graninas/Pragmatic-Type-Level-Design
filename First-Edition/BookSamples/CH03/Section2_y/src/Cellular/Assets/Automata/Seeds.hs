{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Cellular.Assets.Automata.Seeds where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.Boards

-- B2/S
type SeedsStep = 'Step
  '[ 'StateTransition 0 1 '[ 'CellsCount 1 '[2]]  -- "Born rule"
   , 'DefaultTransition 0
   ]

type SeedsRule = 'Rule
  "Seeds"
  "seeds"
  Open2StateBoard
  ('AdjacentsLvl 1)
  SeedsStep

