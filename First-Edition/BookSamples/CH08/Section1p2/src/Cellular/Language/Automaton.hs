{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module Cellular.Language.Automaton where

import GHC.TypeLits


-- Interfaces

-- -- Cell condition

data ICellCondition where
  CellConditionWrapper :: a -> ICellCondition

type family MkCellCondition a :: ICellCondition where
  MkCellCondition a = 'CellConditionWrapper a

-- -- State

data IState where
  StateWrapper :: a -> IState

type family MkState a :: IState where
  MkState a = 'StateWrapper a

-- -- Neighborhood

data INeighborhood where
  NeighborhoodWrapper :: a -> INeighborhood

type family MkNeighborhood a :: INeighborhood where
  MkNeighborhood a = 'NeighborhoodWrapper a

-- -- Rule

data IRule where
  RuleWrapper :: a -> IRule

type family MkRule a :: IRule where
  MkRule a = 'RuleWrapper a

-- Customizable domain model

data CustomStateTransition = StateTransition
  { cstFromState :: IState
  , cstToState   :: IState
  , cstCondition :: ICellCondition
  }

newtype DefaultState = DefState IState

data CustomStep = Step
  { defState    :: DefaultState
  , transitions :: [CustomStateTransition]
  }
