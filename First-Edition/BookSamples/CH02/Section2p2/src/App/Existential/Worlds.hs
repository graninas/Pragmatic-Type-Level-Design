{-# LANGUAGE GADTs #-}
module App.Existential.Worlds where

import Domain.Board (Board)
import Domain.Cell (Cell(..))
import Domain.Automaton ( iterateWorld, loadFromFile, saveToFile, name, Automaton, CellWorld (..) )
import App.Existential.Rules ( RuleImpl )

import qualified Data.Map as Map
import Data.Proxy ( Proxy )


type Generation = Int

data WorldInstance where
  WI :: Automaton rule
     => Generation
     -> CellWorld rule
     -> WorldInstance

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

type WorldIndex = Int
type Worlds = Map.Map WorldIndex WorldInstance


