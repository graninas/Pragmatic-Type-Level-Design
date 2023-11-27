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


data Neighborhood where
  AdjacentsLvl :: Nat -> Neighborhood

type StateIdxNat = Nat
type CountsNat = [Nat]

data DefaultState
  = DefState StateIdxNat

data CellCondition
  = NeighborsCount
  { ccState :: StateIdxNat                  -- what state to count
  , ccQuantity :: CountsNat                 -- how many cells of this state should be
  }                                         -- to activate the condition

data CustomStateTransition
  = StateTransition
  { cstFromState :: StateIdxNat
  , cstToState   :: StateIdxNat
  , cstCondition :: CellCondition
  }

data CustomStep
  = Step
  { csDefaultState :: DefaultState
  , csTransitions :: [CustomStateTransition]
  }

