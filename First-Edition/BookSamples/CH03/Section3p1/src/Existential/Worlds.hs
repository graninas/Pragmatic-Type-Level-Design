{-# LANGUAGE GADTs #-}
module Existential.Worlds where

import Board (Board)
import Cell (Cell(..))
import Automaton ( iterateWorld, loadFromFile, saveToFile, name, Automaton, CellWorld (..) )

import qualified Data.Map as Map
import Data.Proxy ( Proxy )

import Existential.Rules ( RuleImpl )

type Generation = Int

data WorldInstance where
  WI :: Automaton rule
     => Generation
     -> CellWorld rule
     -> WorldInstance

type WorldIndex = Int
type Worlds = Map.Map WorldIndex WorldInstance


