{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
module Domain.AutomatonNG where


import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Domain.Cell (Cell)


data Topology = Open | Torus
type Dimension = Nat
type RuleName = Symbol
type RuleCode = Symbol
type States = Nat
type IsDefault = Bool

data Neighborhood where
  AdjacentsLvl :: Nat -> Neighborhood

data CustomState where
  State :: IsDefault -> Nat -> CustomState

data CustomBoard (states :: [CustomState]) where
  SquareGrid    -- names of val constr should differ
                  -- to avoid name clash
                  -- with kinds (the compiler gets confused)
    :: Topology
    -> [CustomState]
    -> CustomBoard states

data CustomStep where
  Step :: Neighborhood -> [CustomStateTransition] -> CustomStep

data CustomRule (board :: CustomBoard states) where
  Rule
    :: RuleName
    -> RuleCode
    -> CustomBoard states
    -> CustomStep
    -> CustomRule board

data CellCondition where
  CellsCount
    :: Nat            -- what state to count
    -> [Nat]          -- how many cells of this state should be
    -> CellCondition  -- to activate the condition

data CustomStateTransition where
  StateTransition
    :: Nat              -- from state
    -> Nat              -- to state
    -> [CellCondition]  -- neighbors count conditions
    -> CustomStateTransition
  DefaultTransition :: Nat -> CustomStateTransition


data CellWorld (rule :: CustomBoard states)

class IAutomaton
  (rule :: CustomRule (board :: CustomBoard states)) where
  step
    :: Proxy rule
    -> CellWorld board
    -> CellWorld board

class IBoard (board :: CustomBoard (states :: [CustomState])) where
  init :: CellWorld board
  neighborsCount
    :: Proxy states
    -> CellWorld board
    -> Neighborhood
    -> Int

type States2 = '[State True 0, State False 1]

type Open2StateBoard = SquareGrid Open States2

type GoLStep = Step
  (AdjacentsLvl 1)
  '[ StateTransition 0 1 '[CellsCount 1 '[3 ]]   -- "Born rule"
   , StateTransition 1 1 '[CellsCount 1 '[2,3]]  -- "Survive rule"
   , DefaultTransition 0
   ]

type GameOfLife = Rule @States2 "Game of Life" "gol" Open2StateBoard GoLStep


instance IAutomaton GameOfLife where
  step
    :: Proxy GameOfLife
    -> CellWorld board
    -> CellWorld board
  step _ b = let
    -- updCellFunc = makeUpdateFunc
    in b

-- makeUpdateFunc = \pos ->
--   let nsCount = count





-- golStep :: GoL -> GoL
-- golStep (CW board) = CW board'
--   where
--     updateCell :: Coords -> Cell
--     updateCell pos =
--         case (Map.lookup pos board, countAliveNeighbours board pos) of
--             (Just Dead, 3)  -> Alive
--             (Just Alive, 2) -> Alive
--             (Just Alive, 3) -> Alive
--             _               -> Dead
--     board' :: Board
--     board' = Map.mapWithKey (\pos _ -> updateCell pos) board
