{-# LANGUAGE GADTs #-}
module Cellular.App.Valuefied.Worlds where

import Cellular.Domain.Board (Board)
import Cellular.Domain.Cell (Cell(..))
import Cellular.Domain.Automaton ( iterateWorld, loadFromFile, saveToFile, name, Automaton, CellWorld (..) )

import Cellular.App.Valuefied.Rules ( RuleImpl )

import qualified Data.Map as Map
import Data.Proxy ( Proxy )

type Generation = Int

data WorldInstance = WI
  { ruleImpl   :: RuleImpl
  , worldGen   :: Generation
  , worldBoard :: Board
  }

type WorldIndex = Int
type Worlds = Map.Map WorldIndex WorldInstance

