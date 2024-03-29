{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}


{-# LANGUAGE UndecidableInstances #-}
module Cellular.Implementation.Algorithm where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

import Common.NatList

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Language.Integrity


class MakeNeighborhoodLookup (n :: Neighborhood) where
  makeNeighborhoodLookup
    :: Proxy n
    -> Int
    -> Board
    -> GenericCoords
    -> Cells

class
  MakeStep (step :: CustomStep (states :: [CustomState])) where
  makeStep
    :: MakeNeighborhoodLookup neiborhood
    => Proxy step
    -> Proxy (neiborhood :: Neighborhood)
    -> Board
    -> Board

class MakeCellUpdate (ts :: [CustomStateTransition]) where
  makeCellUpdate
    :: Proxy ts
    -> StateIdx                       -- def state
    -> (GenericCoords -> Cells)       -- neighborhood lookup
    -> GenericCoords                  -- current cell
    -> StateIdx                       -- current cell state
    -> StateIdx                       -- new state

class ApplyTransition (t :: CustomStateTransition) where
  applyTransition
    :: Proxy t
    -> Cells
    -> StateIdx                       -- current cell state
    -> Maybe StateIdx

class ApplyCondition (c :: CellCondition) where
  applyCondition :: Proxy c -> Cells -> Bool


instance
  ( KnownNat lvl
  ) =>
  MakeNeighborhoodLookup ('AdjacentsLvl lvl) where
  makeNeighborhoodLookup _ defIdx board coords = let
    ns = generateNeighborhood coords
          $ AdjacentsLvl
          $ fromIntegral
          $ natVal (Proxy @lvl)
    in getCells ns defIdx board


instance
  ( MakeCellUpdate ts
  , Verify (StatesNotEmpty states)   -- FlexibleContexts used here
  , Verify (AtLeastTwoStates states)
  , Verify (StatesAreUnique states)
  , Verify (StateNamesAreUnique states)
  , Verify (DefaultStateIsReal def states)
  , ('DefState defIdx) ~ def
  , KnownNat defIdx
  ) =>
  MakeStep ('Step @states def ts) where
  makeStep _ nProxy board = let
    defIdx = fromIntegral $ natVal $ Proxy @defIdx
    nsLookupF = makeNeighborhoodLookup nProxy defIdx board
    updateF = makeCellUpdate (Proxy @ts) defIdx nsLookupF
    board' = Map.mapWithKey updateF board
    in board'

-- Base case: No transition, return the default state
instance MakeCellUpdate '[] where
  makeCellUpdate _ def _ _ _ = def

-- Inductive case: Try first transition and recur if it doesn't fit
instance
  ( ApplyTransition t
  , MakeCellUpdate ts
  ) =>
  MakeCellUpdate (t ': ts) where
  makeCellUpdate _ def nsLookupF coords oldState = let
    ns = nsLookupF coords
    mbApplied = applyTransition (Proxy @t) ns oldState
    in case mbApplied of
      Just newState -> newState
      Nothing       -> makeCellUpdate (Proxy @ts) def nsLookupF coords oldState

instance
  ( ApplyCondition cond
  , KnownNat from
  , KnownNat to
  ) =>
  ApplyTransition ('StateTransition from to cond) where
  applyTransition _ ns oldState =
    if oldState == fromIntegral (natVal (Proxy @from))
      && applyCondition (Proxy @cond) ns
    then Just (fromIntegral (natVal (Proxy @to)))
    else Nothing

instance
  ( KnownNat cellIdxNat
  , ToIntList counts
  ) =>
  ApplyCondition ('NeighborsCount cellIdxNat counts) where
  applyCondition _ ns =
    let
        target = fromIntegral $ natVal $ Proxy @cellIdxNat
        cnt = length (filter (\(_,idx) -> idx == target) ns)
        counts = toIntList (Proxy @counts)
    in cnt `elem` counts

generateNeighborhood
  :: GenericCoords
  -> Neighborhood
  -> [GenericCoords]
generateNeighborhood coords (AdjacentsLvl 1)
  = filter (/= coords)
  $ mapM (\x -> [x-1, x, x+1]) coords
generateNeighborhood _ _ = error "Neighborhood not implemented for adjacents lvl > 1"

getCells
  :: [GenericCoords]
  -> StateIdx
  -> Board
  -> Cells
getCells ns def board =
  map (\coord ->
    (coord, fromMaybe def (Map.lookup coord board))) ns


------- Dynamic rule step ------


makeStepDyn
  :: DynamicRule
  -> Board
  -> Board
makeStepDyn (DynamicRule _ _ nhDef (DynamicStep defState ts)) board = let
  DefState defIdxNat = defState
  defIdx = fromIntegral defIdxNat
  nsLookupF = makeNeighborhoodLookupDyn defIdx board
  updateF = makeCellUpdateDyn ts defIdx nsLookupF
  board' = Map.mapWithKey updateF board
  in board'

  where

  makeNeighborhoodLookupDyn defIdx board coords = let
    nh = generateNeighborhood coords nhDef
    in getCells nh defIdx board

  makeCellUpdateDyn [] def _ _ oldState = def
  makeCellUpdateDyn (t:ts) def nsLookupF coords oldState = let
    ns = nsLookupF coords
    mbApplied = applyTransitionDyn t ns oldState
    in case mbApplied of
      Just newState -> newState
      Nothing       -> makeCellUpdateDyn ts def nsLookupF coords oldState

  applyTransitionDyn (StateTransition fromIdxNat toIdxNat cond) ns oldState =
    if oldState == fromIntegral fromIdxNat
      && applyConditionDyn cond ns
    then Just (fromIntegral toIdxNat)
    else Nothing

  applyConditionDyn (NeighborsCount cellIdxNat countsNatList) ns =
    let
        target = fromIntegral cellIdxNat
        cnt = length (filter (\(_,idx) -> idx == target) ns)
        counts = map fromIntegral countsNatList
    in cnt `elem` counts
