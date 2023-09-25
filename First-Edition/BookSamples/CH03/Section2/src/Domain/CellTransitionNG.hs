{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
module Domain.CellTransitionNG where


import GHC.TypeLits ( Nat )
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)
import Domain.Automaton (CellWorld)

type StateIdx = Nat

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

data CustomStep where
  Step
    :: [CustomStateTransition]
    -> CustomStep



class ApplyStep (step :: CustomStep) where
  applyStep
    :: Proxy step
    -> StateIdx
    -> [StateIdx]
    -> StateIdx  -- Old state, neighbor states, new state

-- class ApplyTransitions (ts :: [CustomStateTransition]) where
--   applyTransitions :: StateIdx -> [StateIdx] -> StateIdx  -- Old state, neighbor states, new state


-- class ApplyCondition (c :: CellCondition) where
--   applyCondition :: [StateIdx] -> Bool


-- class ApplyTransition (t :: CustomStateTransition) where
--   applyTransition :: StateIdx -> [StateIdx] -> Maybe StateIdx

-- class ApplyConditions (cs :: [CellCondition]) where
--   applyConditions :: [StateIdx] -> Bool





-- instance (ApplyTransitions ts) =>
instance
  ApplyStep ('Step ts) where
  applyStep :: Proxy ('Step ts) -> StateIdx -> [StateIdx] -> StateIdx
  applyStep _ old neighbors =
    error "not implemented"
    -- applyTransitions @ts old neighbors

-- -- Base case: No transition, return the default state
-- instance ApplyTransitions '[] where
--   applyTransitions _ _ = 0  -- Could be made more flexible

-- -- Inductive case: Try first transition and recur if it doesn't fit
-- instance (ApplyTransition t, ApplyTransitions ts) => ApplyTransitions (t ': ts) where
--   applyTransitions old neighbors =
--     case applyTransition @t old neighbors of
--       Just newState -> newState
--       Nothing       -> applyTransitions @ts old neighbors

-- instance (ApplyConditions cs) => ApplyTransition ('StateTransition from to cs) where
--   applyTransition old neighbors =
--     if old == from && applyConditions @cs neighbors then Just to else Nothing

-- instance ApplyTransition ('DefaultTransition to) where
--   applyTransition _ _ = Just to


-- instance ApplyConditions '[] where
--   applyConditions _ = True

-- instance (ApplyCondition c, ApplyConditions cs) => ApplyConditions (c ': cs) where
--   applyConditions neighbors =
--     applyCondition @c neighbors && applyConditions @cs neighbors


-- instance ApplyCondition ('CellsCount targetCount counts) where
--   applyCondition neighbors =
--     let cnt = length (filter (== targetCount) neighbors)
--     in cnt `elem` counts



