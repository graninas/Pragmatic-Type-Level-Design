{-# LANGUAGE DataKinds #-}
module Replicator where

import Automaton ( CellWorld )

import qualified Data.Map as Map

type ReplicatorRule = "Replicator"       -- DataKinds used here
type Replicator = CellWorld ReplicatorRule

{-
-- Alternative:

type Replicator = CellWorld "Replicator"
-}

-- TODO: rules

replicatorStep :: Replicator -> Replicator
replicatorStep = error "Not implemented"
