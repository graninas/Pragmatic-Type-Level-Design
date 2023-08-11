module Diamoeba where

import Cell
import Board ( loadBoardFromFile, saveBoardToFile, Board )
import Automaton ( Automaton(..) )

import qualified Data.Map as Map


data Diamoeba = Diamoeba
  { diamoebaBoard     :: [((Int, Int), Cell)]
  , diamoebaWorldName :: String
  }
  deriving (Show, Eq)

instance Automaton Diamoeba where
  step :: Diamoeba -> Diamoeba                      -- N.B., InstanceSigs is enabled to show sigs
  step = diamoebaStep
  wrap :: Board -> Diamoeba
  wrap board = Diamoeba (Map.toList board) ""
  unwrap :: Diamoeba -> Board
  unwrap (Diamoeba board _) = Map.fromList board


-- TODO: rules

diamoebaStep :: Diamoeba -> Diamoeba
diamoebaStep = error "Not implemented"

