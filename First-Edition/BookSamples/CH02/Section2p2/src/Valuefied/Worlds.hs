{-# LANGUAGE GADTs #-}
module Valuefied.Worlds where

import Board (Board)
import Cell (Cell(..))
import Automaton ( iterateWorld, loadFromFile, saveToFile, name, Automaton, CellWorld (..) )

import Valuefied.Rules ( RuleImpl )

import qualified Data.Map as Map
import Data.Proxy ( Proxy )

type Generation = Int

data WorldInstance = WorldInstance
  { ruleImpl   :: RuleImpl
  , worldGen   :: Generation
  , worldBoard :: Board
  }

type WorldIndex = Int
type Worlds = Map.Map WorldIndex WorldInstance

