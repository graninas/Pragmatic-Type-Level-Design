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
import Domain.BoardNG

data Topology = Open | Torus
type Dimension = Nat
type RuleName = Symbol
type RuleCode = Symbol


data CellWorld rule where
  CW :: Board -> CellWorld rule

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


class IAutomaton
  (rule :: CustomRule
    (board :: CustomBoard)) where
  step
    :: CellWorld rule
    -> CellWorld rule
  step = id


class IWorld
    (rule :: CustomRule
      (board :: CustomBoard)) where
  initWorld :: CellWorld rule
  initWorld = CW Map.empty



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




iterateWorld
  :: forall name code board neighborhood step
   . (MakeStep step, MakeNeighborhoodLookup neighborhood)
  => CellWorld ('Rule name code board neighborhood step)
  -> CellWorld ('Rule name code board neighborhood step)
iterateWorld (CW board) = let
  stepF = makeStep (Proxy @step) (Proxy @neighborhood)
  in CW (stepF board)
