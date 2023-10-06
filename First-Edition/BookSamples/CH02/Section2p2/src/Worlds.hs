{-# LANGUAGE GADTs #-}
module Worlds where

import Board (Board)
import Cell (Cell(..))
import Automaton ( iterateWorld, loadFromFile, saveToFile, name, Automaton, CellWorld (..) )

import qualified Data.Map as Map
import Data.Proxy ( Proxy )

data WorldInstance where
  WorldInstance :: Automaton rule => Proxy rule -> CellWorld rule -> WorldInstance

  -- There is some difference between that ^ and this V
    -- WorldInstance :: (Automaton rule => CellWorld rule) -> WorldInstance
  --
  -- This variant doesn't work for:
  --   case worlds2 of
  --     [WorldInstance gol2, WorldInstance seeds2] -> do
  --      print (name gol2)
  --      print (name seeds1)
  -- It says "Automaton should be defined for WorldInstance"

type WorldIndex = Int
type Worlds = Map.Map WorldIndex WorldInstance

