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

data CellCondition
  = CellsCount
  { ccState :: StateIdxNat             -- what state to count
  , ccQuantity :: CountsNat            -- how many cells of this state should be
  }                                    -- to activate the condition

data CustomStateTransition
  = StateTransition
  { cstFromState :: StateIdxNat             -- from state
  , cstToState :: StateIdxNat               -- to state
  , cstConditions :: [CellCondition]        -- neighbors count conditions
  }
  | DefaultTransition
  { cstDefaultState :: StateIdxNat
  }

data CustomStep
  = Step
  { csTransitions :: [CustomStateTransition]
  }

