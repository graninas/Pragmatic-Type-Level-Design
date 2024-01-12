{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module App.Existential.Worlds where

import Cellular.Automaton
import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.Seeds
import Cellular.Assets.Automata.Replicator
import App.Existential.Rules ( RuleImpl )

import qualified Data.Map as Map
import Data.Proxy ( Proxy )


type Generation = Int

data WorldInstance where
  WI :: IAutomaton rule
     => Generation
     -> CellWorld rule
     -> WorldInstance

type WorldIndex = Int
type Worlds = Map.Map WorldIndex WorldInstance


