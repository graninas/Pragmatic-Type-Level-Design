{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Cellular.App.Existential.Worlds where

import Cellular.Automaton (IAutomaton)
import Cellular.Language.Automaton (CellWorld)

import qualified Data.Map as Map


type Generation = Int

data WorldInstance where
  WI :: IAutomaton rule
     => Generation
     -> CellWorld rule
     -> WorldInstance

type WorldIndex = Int
type Worlds = Map.Map WorldIndex WorldInstance


