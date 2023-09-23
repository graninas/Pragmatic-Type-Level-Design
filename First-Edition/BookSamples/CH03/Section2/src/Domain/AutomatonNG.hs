{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.AutomatonNG where


import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Domain.Cell (Cell)


data Topology = Open | Torus
type Dimension = Nat
type RuleName = Symbol
type RuleCode = Symbol
type States = Nat

data Neighborhood where
  AdjacentsLvl :: Nat -> Neighborhood

data CustomBoard where
  SquareGrid    -- names of val constr should differ
                  -- to avoid name clash
                  -- with kinds (the compiler gets confused)
    :: Topology
    -> CustomBoard

data CustomRule (board :: CustomBoard) where
  Rule
    :: RuleName
    -> RuleCode
    -> CustomStep
    -> CustomRule board

data CellWorld (board :: CustomBoard)

class IAutomaton (board :: CustomBoard) (rule :: CustomRule board) where
  step
    :: Proxy rule
    -> CellWorld board
    -> CellWorld board

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

data CustomStep where
  Step :: Neighborhood -> [CustomStateTransition] -> CustomStep

data Lines3S
type Lines3SBoard = CustomBoard

type StandardBoard = SquareGrid Open

type GoLStep = Step
  (AdjacentsLvl 1)
  '[ StateTransition 0 1 '[CellsCount 1 '[3 ]]   -- "Born rule"
   , StateTransition 1 1 '[CellsCount 1 '[2,3]]  -- "Survive rule"
   , DefaultTransition 0
   ]
type GameOfLife = Rule "Game of Life" "gol" GoLStep


class IBoard (board :: CustomBoard) where
  init :: CellWorld board
  neighborsCount :: CellWorld board -> Neighborhood -> Int

  -- init :: Proxy GameOfLife -> CellWorld StandardBoard
  -- init _ = undefined




instance IAutomaton StandardBoard GameOfLife where
  step
    :: Proxy GameOfLife
    -> CellWorld StandardBoard
    -> CellWorld StandardBoard
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
