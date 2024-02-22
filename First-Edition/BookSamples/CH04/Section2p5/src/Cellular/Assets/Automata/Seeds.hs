{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Assets.Automata.Seeds where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.LifeLike
import Common.NonEmptyList


-- Seeds (B2/S)
type Neighbors2 = 'NeighborsCount A ('List1 2 '[])

type SeedsStep = 'Step ('DefState D)
  '[ 'StateTransition D A Neighbors2  -- "Born rule"
   ]

type SeedsRule = 'Rule
  @LifeLikeStates
  "Seeds"
  "seeds"
  ('AdjacentsLvl 1)
  SeedsStep

