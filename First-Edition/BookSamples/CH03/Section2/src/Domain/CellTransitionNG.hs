{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Domain.CellTransitionNG where


import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

import Domain.BoardNG


type StateIdxNat = Nat

data CellCondition where
  CellsCount
    :: StateIdxNat    -- what state to count
    -> [Nat]          -- how many cells of this state should be
    -> CellCondition  -- to activate the condition

data CustomStateTransition where
  StateTransition
    :: StateIdxNat      -- from state
    -> StateIdxNat      -- to state
    -> [CellCondition]  -- neighbors count conditions
    -> CustomStateTransition
  DefaultTransition
    :: StateIdxNat
    -> CustomStateTransition

data CustomStep where
  Step
    :: [CustomStateTransition]
    -> CustomStep



class MakeStep (step :: CustomStep) where
  makeStep :: Proxy step -> (Board -> Board)

class MakeCellUpdate (ts :: [CustomStateTransition]) where
  makeCellUpdate
    :: Proxy ts
    -> (GenericCoords -> Neighborhood -> Board -> StateIdxNat)

class ApplyTransition (t :: CustomStateTransition) where
  applyTransition
    :: Proxy t
    -> Cells -> Maybe StateIdxNat

-- class ApplyCondition (c :: CellCondition) where
--   applyCondition :: [StateIdx] -> Bool


-- class ApplyConditions (cs :: [CellCondition]) where
--   applyConditions :: [StateIdx] -> Bool





instance (MakeCellUpdate ts) =>
  MakeStep ('Step ts) where
  makeStep _ board = let
    updateF = makeCellUpdate (Proxy @ts)
    in board

-- Base case: No transition, return the default state
instance MakeCellUpdate '[] where
  makeCellUpdate _ _ _ _ = 0    -- Default state

-- Inductive case: Try first transition and recur if it doesn't fit
instance (ApplyTransition t, MakeCellUpdate ts)
  => MakeCellUpdate (t ': ts) where

  makeCellUpdate _ coords neighborhood board = let
    oldState = fromMaybe 0 $ Map.lookup coords board
    ns = neighbors coords neighborhood board
    -- mbApplied = applyTransition (Proxy @t) ns oldState
    mbApplied = Nothing
    in case mbApplied of
      Just newState -> newState
      Nothing       -> makeCellUpdate (Proxy @ts) coords neighborhood board



instance
  -- (ApplyConditions cs) =>
  (KnownNat from, KnownNat to)
  => ApplyTransition ('StateTransition from to cs) where
  applyTransition cells oldState = error "not implemented"
    -- if True
    -- -- if oldState == fromIntegral (natVal (Proxy @from))
    --   -- && applyConditions @cs neighbors
    -- -- then Just (fromIntegral (natVal (Proxy @to)))
    -- then Nothing
    -- else Nothing

instance
  KnownNat to =>
  ApplyTransition ('DefaultTransition to) where
  applyTransition _ _ = Just $ fromIntegral $ natVal $ Proxy @to


-- instance ApplyConditions '[] where
--   applyConditions _ = True

-- instance (ApplyCondition c, ApplyConditions cs) => ApplyConditions (c ': cs) where
--   applyConditions neighbors =
--     applyCondition @c neighbors && applyConditions @cs neighbors


-- instance ApplyCondition ('CellsCount targetCount counts) where
--   applyCondition neighbors =
--     let cnt = length (filter (== targetCount) neighbors)
--     in cnt `elem` counts



