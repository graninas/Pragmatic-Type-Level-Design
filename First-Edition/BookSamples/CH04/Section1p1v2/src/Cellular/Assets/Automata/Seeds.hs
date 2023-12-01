{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Assets.Automata.Seeds where

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.Boards
import Cellular.Assets.Automata.LifeLike
import Common.NonEmptyList


-- Seeds (B2/S)
type SeedsStep = 'Step ('DefState D)
  '[ 'StateTransition D A ('NeighborsCount A ('List1 2 '[]))
   ]

type SeedsRule = 'Rule
  @LifeLikeStates
  "Seeds"
  "seeds"
  OpenBoard
  ('AdjacentsLvl 1)
  SeedsStep

