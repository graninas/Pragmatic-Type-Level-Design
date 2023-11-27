{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Cellular.Assets.Automata.Seeds where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.Boards
import Cellular.Assets.Automata.LifeLike


-- B2/S
type SeedsStep = 'Step ('DefState D)
  '[ 'StateTransition D A ('NeighborsCount A '[2])  -- "Born rule"
   ]

type SeedsRule = 'Rule
  "Seeds"
  "seeds"
  Open2StateBoard
  ('AdjacentsLvl 1)
  SeedsStep

