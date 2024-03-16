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


-- | Static & dynamic Neighborhood level definition
data Neighborhood where
  AdjacentsLvl :: Nat -> Neighborhood

-- | Static & dynamic integer state identifier
type StateIdxNat = Nat

-- | Static string state name
type StateNameSymb = Symbol

-- | Static & dynamic list of quantities
type CountsNat = [Nat]

-- | Static & dynamic default state (acts like a newtype)
newtype DefaultState = DefState StateIdxNat

-- | Static & dynamic cell condition
data CellCondition
  = NeighborsCount
  { ccState :: StateIdxNat                  -- what state to count
  , ccQuantity :: CountsNat                 -- how many cells of this state should be
  }                                         -- to activate the condition

-- | Static & dynamic transition from cell to cell
data CustomStateTransition
  = StateTransition
  { cstFromState :: StateIdxNat
  , cstToState   :: StateIdxNat
  , cstCondition :: CellCondition
  }

-- | Static type-level cell state
data CustomState
  = State
  { csName     :: StateNameSymb
  , csStateIdx :: StateIdxNat
  }

-- | Static type-level step
data CustomStep (states :: [CustomState])
  = Step
  { csDefaultState :: DefaultState
  , csTransitions  :: [CustomStateTransition]
  }

-- | Alternatively, custom step as a tuple:
type CustomStepTuple = (DefaultState, [CustomStateTransition])



-- | Dynamic step
data DynamicStep
  = DynamicStep
  { dsDefaultState :: DefaultState
  , dsTransitions  :: [CustomStateTransition]
  }
