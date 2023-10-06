-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
module MkWorld where

import Board (Board)
import Cell (Cell(..))
import Automaton ( iterateWorld, loadFromFile, saveToFile, name, Automaton, CellWorld (..) )
import GameOfLife
import Seeds
import Replicator

import qualified Data.Map as Map

data MkWorld where
  MkWorld :: Automaton rule => CellWorld rule -> MkWorld

  -- There is some difference between that ^ and this V
    -- MkWorld :: (Automaton rule => CellWorld rule) -> MkWorld
  --
  -- This variant doesn't work for:
  --   case worlds2 of
  --     [MkWorld gol2, MkWorld seeds2] -> do
  --      print (name gol2)
  --      print (name seeds1)
  -- It says "Automaton should be defined for MkWorld"

type Worlds = [MkWorld]

worlds
  :: GoL
  -> Seeds
  -> Worlds
worlds gol seeds = [MkWorld gol, MkWorld seeds]
