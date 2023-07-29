module GameOfLife where

import Board ( loadBoardFromFile, saveBoardToFile, Board )
import Automaton ( Automaton(..) )

import qualified Data.Map as Map


newtype GoL = GoL Board
  deriving (Show, Eq)

-- TODO: rules

instance Automaton GoL where
  step :: GoL -> GoL                      -- N.B., InstanceSigs is enabled to show sigs
  step = golStep
  wrap :: Board -> GoL
  wrap = GoL
  unwrap :: GoL -> Board
  unwrap (GoL board) = board

golStep :: GoL -> GoL
golStep = error "Not implemented"

