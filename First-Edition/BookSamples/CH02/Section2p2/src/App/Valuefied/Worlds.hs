{-# LANGUAGE GADTs #-}
module App.Valuefied.Worlds where

import Domain.Board (Board)
import Domain.Cell (Cell(..))
import Domain.Automaton ( iterateWorld, loadFromFile, saveToFile, name, Automaton, CellWorld (..) )

import App.Valuefied.Rules ( RuleImpl )

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

