{-# LANGUAGE GADTs #-}
module Existential.Worlds where

import Board (Board)
import Cell (Cell(..))
import Automaton ( iterateWorld, loadFromFile, saveToFile, name, Automaton, CellWorld (..) )

import qualified Data.Map as Map
import Data.Proxy ( Proxy )

type Generation = Int

data WorldInstance where
  WorldInstance
    :: Automaton rule
    => Proxy rule
    -> Generation
    -> CellWorld rule
    -> WorldInstance

  -- There is some difference between that ^ and this V
  --
  -- WorldInstance
  --   :: (Automaton rule
  --   => Proxy rule
  --   -> Generation
  --   -> CellWorld rule)
  --   -> WorldInstance

  -- This variant doesn't work for:
  --
  --   case worlds2 of
  --     [WorldInstance proxy1 _ _, WorldInstance proxy2 _ _] -> do
  --      print (name proxy1)
  --      print (name proxy2)
  --
  -- It says "Automaton should be defined for WorldInstance"

type WorldIndex = Int
type Worlds = Map.Map WorldIndex WorldInstance

