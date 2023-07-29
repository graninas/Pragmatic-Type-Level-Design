module Seeds where

import Board ( loadBoardFromFile, saveBoardToFile, Board )
import Automaton ( Automaton(..) )

import qualified Data.Map as Map


newtype Seeds = Seeds Board
  deriving (Show, Eq)

instance Automaton Seeds where
  step :: Seeds -> Seeds                      -- N.B., InstanceSigs is enabled to show sigs
  step = seedsStep
  wrap :: Board -> Seeds
  wrap = Seeds
  unwrap :: Seeds -> Board
  unwrap (Seeds board) = board


-- TODO: rules

seedsStep :: Seeds -> Seeds
seedsStep = error "Not implemented"
