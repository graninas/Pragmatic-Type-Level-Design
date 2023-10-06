module Replicator where

import Board ( loadBoardFromFile, saveBoardToFile, Board )
import Automaton ( Automaton(..) )

import qualified Data.Map as Map


newtype Replicator = Replicator Board
  deriving (Show, Eq)

instance Automaton Replicator where
  step :: Replicator -> Replicator              -- N.B., InstanceSigs is enabled to show sigs
  step = replicatorStep
  wrap :: Board -> Replicator
  wrap = Replicator
  unwrap :: Replicator -> Board
  unwrap (Replicator board) = board

-- TODO: rules

replicatorStep :: Replicator -> Replicator
replicatorStep = error "Not implemented"
