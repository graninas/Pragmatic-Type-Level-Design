{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Domain.AutomatonNG where


import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

data Topology = Open | Torus
type Dimension = Nat
type RuleName = Symbol
type RuleCode = Symbol
type StateIdx = Nat
type States = Nat

data Neighborhood where
  AdjacentsLvl :: Nat -> Neighborhood

data CustomState where
  State :: Nat -> CustomState

type CustomStates = [CustomState]

data CustomStep (states :: CustomStates) where
  Step
    :: [CustomStateTransition]
    -> CustomStep states

data CustomBoard (states :: CustomStates) where
  SquareGrid      -- names of val constr should differ
                  -- to avoid name clash
                  -- with kinds (the compiler gets confused)
    :: Topology
    -- -> CustomStates
    -> CustomBoard states

data CustomRule (board :: CustomBoard states) where
  Rule
    :: RuleName
    -> RuleCode
    -> CustomBoard states
    -> Neighborhood
    -> CustomStep states
    -> CustomRule board

data CellCondition where
  CellsCount
    :: StateIdx       -- what state to count
    -> [Nat]          -- how many cells of this state should be
    -> CellCondition  -- to activate the condition

data CustomStateTransition where
  StateTransition
    :: StateIdx         -- from state
    -> StateIdx         -- to state
    -> [CellCondition]  -- neighbors count conditions
    -> CustomStateTransition
  DefaultTransition
    :: StateIdx
    -> CustomStateTransition

type GenericCoords = [Int]
type Board = Map.Map GenericCoords StateIdx

data CellWorld rule where
  CW :: Board -> CellWorld rule

class IAutomaton
  (rule :: CustomRule
    (board :: CustomBoard
      (states :: CustomStates))) where
  step
    :: CellWorld rule
    -> CellWorld rule
  step = id
  neighbors
    :: GenericCoords
    -> Neighborhood                     -- should be taken from rule
    -> CellWorld rule
    -> [(GenericCoords, StateIdx)]
  neighbors coords nsDef world = let
    ns = generateNeighborhood coords nsDef world
    -- def = defState Proxy -- TODO
    in getCells ns 0 world
-- TODO
-- class IState (states :: CustomStates) where
--   defState :: Proxy states -> StateIdx
--   defState _ = 0                      -- TODO

class IWorld
    (rule :: CustomRule
      (board :: CustomBoard
        (states :: CustomStates))) where
  initWorld :: CellWorld rule
  initWorld = CW Map.empty


generateNeighborhood
  :: GenericCoords
  -> Neighborhood
  -> CellWorld rule
  -> [GenericCoords]
generateNeighborhood coords (AdjacentsLvl 1) _
  = filter (/= coords)
  $ mapM (\x -> [x-1, x, x+1]) coords
generateNeighborhood _ _ _ = error "Neighborhood not implemented for adjacents lvl > 1"

getCells
  :: [GenericCoords]
  -> StateIdx
  -> CellWorld rule
  -> [(GenericCoords, StateIdx)]
getCells ns def (CW board) =
  map (\coord ->
    (coord, fromMaybe def (Map.lookup coord board))) ns

-- -------------------------------------------------


type States2 = '[State 0, State 1]


-- UndecidableInstances here
type family StatesCount (states :: [CustomState]) :: Nat where
  StatesCount '[] = 0
  StatesCount (_ ': xs) = 1 + StatesCount xs   -- TypeOperators here

type Open2StateBoard = SquareGrid Open         -- Type application to types

type GoLStep = Step
  '[ StateTransition 0 1 '[CellsCount 1 '[3 ]]   -- "Born rule"
   , StateTransition 1 1 '[CellsCount 1 '[2,3]]  -- "Survive rule"
   , DefaultTransition 0
   ]

type GameOfLife = Rule @States2       -- TODO: @States2 Seems strange. Redesign
  "Game of Life"
  "gol"
  Open2StateBoard
  (AdjacentsLvl 1)
  GoLStep



instance IWorld GameOfLife where

instance IAutomaton GameOfLife where
  step
    :: CellWorld GameOfLife
    -> CellWorld GameOfLife
  step (CW b) = let
    -- updCellFunc = makeUpdateFunc
    in CW b



-- instance IWorld Int where        -- unable to define for invalid
-- instance IAutomaton Int where    -- unable to define for invalid
