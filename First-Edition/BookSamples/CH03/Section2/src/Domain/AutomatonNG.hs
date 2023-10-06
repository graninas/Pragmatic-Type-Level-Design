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
type States = Nat

data Neighborhood where
  AdjacentsLvl :: Nat -> Neighborhood

data CustomState where
  State :: Nat -> CustomState

type CustomStates = [CustomState]

data CustomBoard (states :: CustomStates) where
  SquareGrid      -- names of val constr should differ
                  -- to avoid name clash
                  -- with kinds (the compiler gets confused)
    :: Topology
    -> CustomStates
    -> CustomBoard states

data CustomStep (states :: CustomStates) where
  Step
    :: Neighborhood
    -> [CustomStateTransition]
    -> CustomStep states

data CustomRule (board :: CustomBoard states) where
  Rule
    :: RuleName
    -> RuleCode
    -> CustomBoard states
    -> CustomStep states
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
  DefaultTransition
    :: Nat
    -> CustomStateTransition

type GenericCoords = [Int]

data CellWorld (board :: CustomBoard states) where
  CW :: Map.Map GenericCoords Nat -> CellWorld board

class IAutomaton (rule :: CustomRule board) where
  step
    :: Proxy rule
    -> CellWorld board
    -> CellWorld board

class IBoard (board :: CustomBoard states) where
  init :: CellWorld board
  neighbors
    :: Proxy states
    -> CustomState
    -> GenericCoords
    -> Neighborhood
    -> CellWorld board
    -> [(GenericCoords, Nat)]

-- -------------------------------------------------


type States2 = '[State 0, State 1]


-- UndecidableInstances are used here
type family StatesCount (states :: [CustomState]) :: Nat where
  StatesCount '[] = 0
  StatesCount (_ ': xs) = 1 + StatesCount xs   -- TypeOperators here

type Open2StateBoard = SquareGrid @States2 Open States2   -- Type application to types

type GoLStep = Step
  (AdjacentsLvl 1)
  '[ StateTransition 0 1 '[CellsCount 1 '[3 ]]   -- "Born rule"
   , StateTransition 1 1 '[CellsCount 1 '[2,3]]  -- "Survive rule"
   , DefaultTransition 0
   ]

type GameOfLife = Rule
  "Game of Life"
  "gol"
  Open2StateBoard
  GoLStep







-- makeNeighbours coords nsDef









instance IBoard Open2StateBoard where
  init :: CellWorld Open2StateBoard
  init = CW Map.empty
  neighbors
    :: Proxy states
    -> CustomState
    -> GenericCoords
    -> Neighborhood
    -> CellWorld board
    -> [(GenericCoords, Nat)]
  neighbors states state coords nsDef world = let
    ns = generateNeighborhood coords nsDef world
    def = getDefaultState states
    cs = getCells ns def world
    in filter (thisState state) cs

getDefaultState :: Proxy states -> Nat
getDefaultState _ = 0                     -- TODO

generateNeighborhood
  :: GenericCoords
  -> Neighborhood
  -> CellWorld board
  -> [GenericCoords]
generateNeighborhood coords (AdjacentsLvl 1) _
  = filter (/= coords)
  $ mapM (\x -> [x-1, x, x+1]) coords
generateNeighborhood _ _ _ = error "Neighborhood not implemented"

getCells
  :: [GenericCoords]
  -> Nat
  -> CellWorld board
  -> [(GenericCoords, Nat)]
getCells ns def (CW board) =
  map (\coord ->
    (coord, fromMaybe def (Map.lookup coord board))) ns

thisState _ = undefined


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
