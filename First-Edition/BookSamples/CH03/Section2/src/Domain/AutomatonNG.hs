{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Domain.AutomatonNG where


import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

import Domain.CellTransitionNG

data Topology = Open | Torus
type Dimension = Nat
type RuleName = Symbol
type RuleCode = Symbol

data Neighborhood where
  AdjacentsLvl :: Nat -> Neighborhood

data CustomBoard where
  SquareGrid      -- names of val constr should differ
                  -- to avoid name clash
                  -- with kinds (the compiler gets confused)
    :: Topology
    -> CustomBoard

data CustomRule
  (board :: CustomBoard) where
  Rule
    :: RuleName
    -> RuleCode
    -> CustomBoard
    -> Neighborhood
    -> CustomStep
    -> CustomRule board

type GenericCoords = [Int]
type Board = Map.Map GenericCoords StateIdx

data CellWorld rule where
  CW :: Board -> CellWorld rule



class IAutomaton
  (rule :: CustomRule
    (board :: CustomBoard)) where
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

class IWorld
    (rule :: CustomRule
      (board :: CustomBoard)) where
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


-- instance IWorld Int where        -- unable to define for invalid
-- instance IAutomaton Int where    -- unable to define for invalid




-- TODO
-- class IState (states :: CustomStates) where
--   defState :: Proxy states -> StateIdx
--   defState _ = 0                      -- TODO

-- data CustomState where
--   State :: Nat -> CustomState

-- type CustomStates = [CustomState]

-- type States2 = '[State 0, State 1]

-- -- UndecidableInstances here
-- type family StatesCount (states :: [CustomState]) :: Nat where
--   StatesCount '[] = 0
--   StatesCount (_ ': xs) = 1 + StatesCount xs   -- TypeOperators here




runStep
  :: forall a b c d step
   . ApplyStep step
  => CellWorld ('Rule a b c d step)
  -> CellWorld ('Rule a b c d step)
runStep (CW board) = let
  result = applyStep (Proxy @step) 0 []
  in CW board
