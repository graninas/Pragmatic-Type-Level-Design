{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Cellular.Language.Algorithm where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

import Cellular.Language.Board


type StateIdxNat = Nat
type CountsNat = [Nat]

data Neighborhood where
  AdjacentsLvl :: Nat -> Neighborhood

data CellCondition where
  CellsCount
    :: StateIdxNat             -- what state to count
    -> CountsNat               -- how many cells of this state should be
    -> CellCondition           -- to activate the condition

data CustomStateTransition where
  StateTransition
    :: StateIdxNat             -- from state
    -> StateIdxNat             -- to state
    -> [CellCondition]         -- neighbors count conditions
    -> CustomStateTransition
  DefaultTransition
    :: StateIdxNat
    -> CustomStateTransition

data CustomStep where
  Step
    :: [CustomStateTransition]
    -> CustomStep

