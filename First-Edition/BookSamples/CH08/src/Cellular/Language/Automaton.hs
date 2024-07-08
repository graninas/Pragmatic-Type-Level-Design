{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module Cellular.Language.Automaton where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)


-- Interfaces

-- data IBoard where
--   BoardWrapper :: a -> IBoard

-- type family MkBoard a :: IBoard where
--   MkBoard a = BoardWrapper a

data ICellCondition where
  CellConditionWrapper :: a -> ICellCondition

type family MkCellCondition a :: ICellCondition where
  MkCellCondition a = CellConditionWrapper a

data IState where
  StateWrapper :: a -> IState

type family MkState a :: IState where
  MkState a = StateWrapper a

data IRule where
  RuleWrapper :: a -> IRule

type family MkRule a :: IRule where
  MkRule a = RuleWrapper a


-- Implementations

data NeighborsCountImpl
  (state :: IState)
  (counts :: [Nat])
type NeighborsCount s c =
  MkCellCondition (NeighborsCountImpl s c)

data CustomStateTransition = StateTransition
  { cstFromState :: IState
  , cstToState   :: IState
  , cstCondition :: ICellCondition
  }

data CustomStep = Step
  { defState :: IState
  , transitions :: [CustomStateTransition]
  }

data StateImpl
  (name :: Symbol)
type State n = MkState (StateImpl n)

data RuleImpl
  (name :: Symbol)
  (code :: Symbol)
  (step :: CustomStep)
type Rule n c s = MkRule (RuleImpl n c s)


type D = State "Dead"
type A = State "Alive"

type Neighbors3  = NeighborsCount A '[3  ]
type Neighbors23 = NeighborsCount A '[2,3]

type GoLStep = Step D
  '[ 'StateTransition D A Neighbors3
   , 'StateTransition A A Neighbors23
   ]

type GoLRule = Rule
  "Game of Life"
  "gol"
  GoLStep
