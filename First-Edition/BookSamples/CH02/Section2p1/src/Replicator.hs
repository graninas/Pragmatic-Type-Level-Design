{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Replicator where

import Board ( loadBoardFromFile, saveBoardToFile, Board )
import Automaton ( Automaton(..), CellWorld )

import qualified Data.Map as Map

type ReplicatorRule = "Replicator"       -- DataKinds used here
type Replicator = CellWorld ReplicatorRule

{-
-- Alternative:

type Replicator = CellWorld "Replicator"
-}

instance Automaton ReplicatorRule where  -- FlexibleInstances used here
  step :: Replicator -> Replicator       -- InstanceSigs is enabled to show sigs
  step = replicatorStep

-- TODO: rules

replicatorStep :: Replicator -> Replicator
replicatorStep = error "Not implemented"
