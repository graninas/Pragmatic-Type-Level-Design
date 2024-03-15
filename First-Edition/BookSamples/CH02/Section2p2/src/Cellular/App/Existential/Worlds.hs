{-# LANGUAGE GADTs #-}
module Cellular.App.Existential.Worlds where

import Cellular.Domain.Automaton (Automaton, CellWorld)

import qualified Data.Map as Map


type Generation = Int

data WorldInstance where
  WI :: Automaton rule
     => Generation
     -> CellWorld rule
     -> WorldInstance

type WorldIndex = Int
type Worlds = Map.Map WorldIndex WorldInstance

  -- There is some difference between that ^ and this V
  --
  -- WI
  --   :: (Automaton rule
  --   => RuleImpl
  --   -> Generation
  --   -> CellWorld rule)
  --   -> WorldInstance

  -- This variant doesn't work for:
  --
  --   case worlds2 of
  --     [WorldInstance ri1 _ _, WorldInstance ri2 _ _] -> do
  --     ...
  --
  -- It says "Automaton should be defined for WorldInstance"


