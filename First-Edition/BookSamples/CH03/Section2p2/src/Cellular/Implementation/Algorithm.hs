{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Cellular.Implementation.Algorithm where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

import Common.NatList

import Cellular.Language.Board
import Cellular.Language.Algorithm


class MakeNeighborhoodLookup (n :: Neighborhood) where
  makeNeighborhoodLookup
    :: Proxy n
    -> Board
    -> GenericCoords
    -> Cells

class MakeStep (step :: CustomStep) where
  makeStep
    :: MakeNeighborhoodLookup neiborhood
    => Proxy step
    -> Proxy (neiborhood :: Neighborhood)
    -> Board
    -> Board

class MakeCellUpdate (ts :: [CustomStateTransition]) where
  makeCellUpdate
    :: Proxy ts
    -> (GenericCoords -> Cells)       -- neighborhood lookup
    -> GenericCoords                  -- current cell
    -> StateIdx                       -- current cell state
    -> StateIdx                       -- new state

class ApplyTransition (t :: CustomStateTransition) where
  applyTransition
    :: Proxy t
    -> Cells
    -> StateIdx
    -> Maybe StateIdx

class ApplyCondition (c :: CellCondition) where
  applyCondition :: Proxy c -> Cells -> Bool


instance
  KnownNat lvl =>
  MakeNeighborhoodLookup ('AdjacentsLvl lvl) where
  makeNeighborhoodLookup _ board coords = let
    ns = generateNeighborhood coords
          $ AdjacentsLvl
          $ fromIntegral
          $ natVal (Proxy @lvl)
    in getCells ns 0 board        -- Default value


instance
  MakeCellUpdate ts =>
  MakeStep ('Step ts) where
  makeStep _ nProxy board = let
    nsLookupF = makeNeighborhoodLookup nProxy board
    updateF = makeCellUpdate (Proxy @ts) nsLookupF
    board' = Map.mapWithKey updateF board
    in board'

-- Base case: No transition, return the default state
instance MakeCellUpdate '[] where
  makeCellUpdate _ _ _ _ = 0    -- Default state

-- Inductive case: Try first transition and recur if it doesn't fit
instance (ApplyTransition t, MakeCellUpdate ts) =>
  MakeCellUpdate (t ': ts) where
  makeCellUpdate _ nsLookupF coords oldState = let
    ns = nsLookupF coords
    mbApplied = applyTransition (Proxy @t) ns oldState
    in case mbApplied of
      Just newState -> newState
      Nothing       -> makeCellUpdate (Proxy @ts) nsLookupF coords oldState

instance
  (ApplyCondition cond, KnownNat from, KnownNat to) =>
  ApplyTransition ('StateTransition from to cond) where
  applyTransition _ ns oldState =
    if oldState == fromIntegral (natVal (Proxy @from))
      && applyCondition (Proxy @cond) ns
    then Just (fromIntegral (natVal (Proxy @to)))
    else Nothing

instance
  KnownNat to =>
  ApplyTransition ('DefaultTransition to) where
  applyTransition _ _ _
    = Just
    $ fromIntegral
    $ natVal
    $ Proxy @to

instance
  (KnownNat cellIdxNat, ToIntList counts) =>
  ApplyCondition ('NeighborsCount cellIdxNat counts) where
  applyCondition _ ns =
    let
        target = fromIntegral $ natVal $ Proxy @cellIdxNat
        cnt = length (filter (\(_,idx) -> idx == target) ns)
        counts = toIntList (Proxy @counts)
    in cnt `elem` counts


neighbors
  :: GenericCoords
  -> Neighborhood
  -> Board
  -> Cells
neighbors coords nsDef board = let
  ns = generateNeighborhood coords nsDef
  in getCells ns 0 board    -- TODO: default cell state

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

