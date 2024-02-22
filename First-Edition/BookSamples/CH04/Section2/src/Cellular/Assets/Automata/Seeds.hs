{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Assets.Automata.Seeds where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.LifeLike


-- Seeds (B2/S)
type Neighbors2 = 'NeighborsCount A '[2]

type SeedsStep = 'Step @LifeLikeStates ('DefState D)
  '[ 'StateTransition D A Neighbors2  -- "Born rule"
   ]

type SeedsRule = 'Rule
  "Seeds"
  "seeds"
  ('AdjacentsLvl 1)
  SeedsStep

