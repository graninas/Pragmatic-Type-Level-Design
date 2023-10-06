module Automata where

import Board ( Board )


newtype Seeds = Seeds Board
  deriving (Show, Eq)

newtype Replicator = Replicator Board
  deriving (Show, Eq)


seedsStep :: Seeds -> Seeds
seedsStep = error "Not implemented"

replicatorStep :: Replicator -> Replicator
replicatorStep = error "Not implemented"
