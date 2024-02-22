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


-- | Neighborhood level definition
data Neighborhood where
  AdjacentsLvl :: Nat -> Neighborhood

-- | Integer state identifier
type StateIdxNat = Nat

-- | String state name
type StateNameSymb = Symbol

-- | List of quantities
type CountsNat = [Nat]

-- | Default state (acts like a newtype)
newtype DefaultState = DefState StateIdxNat

-- | Cell condition
data CellCondition
  = NeighborsCount
  { ccState :: StateIdxNat                  -- what state to count
  , ccQuantity :: CountsNat                 -- how many cells of this state should be
  }                                         -- to activate the condition

-- | Transition from cell to cell
data CustomStateTransition
  = StateTransition
  { cstFromState :: StateIdxNat
  , cstToState   :: StateIdxNat
  , cstCondition :: CellCondition
  }

-- | Cell state definition
data CustomState
  = State
  { csName     :: StateNameSymb
  , csStateIdx :: StateIdxNat
  }

-- | Automaton step definition
data CustomStep
  = Step
  { csDefaultState :: DefaultState
  , csTransitions  :: [CustomStateTransition]
  }

-- | Alternatively, custom step as a tuple:
type CustomStepTuple = (DefaultState, [CustomStateTransition])
