{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cellular.App.Existential.Worlds where

import Cellular.Automaton (IAutomaton)
import Cellular.Language.Automaton (CellWorld, DynamicRule, CustomRule(..))

import qualified Data.Map as Map


type Generation = Int

data WorldInstance where
  WI
    :: IAutomaton () rule
    => Generation
    -> CellWorld rule
    -> WorldInstance
  DynWI
    :: IAutomaton DynamicRule 'DynRule
    => DynamicRule
    -> Generation
    -> CellWorld 'DynRule
    -> WorldInstance


type WorldIndex = Int
type Worlds = Map.Map WorldIndex WorldInstance


