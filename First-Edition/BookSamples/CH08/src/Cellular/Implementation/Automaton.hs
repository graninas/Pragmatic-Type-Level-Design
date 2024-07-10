{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module Cellular.Implementation.Automaton where

import Cellular.Language.Automaton

import GHC.TypeLits


-- -- Implementations

data NeighborsCountImpl
  (state :: IState)
  (counts :: [Nat])
type NeighborsCount s c =
  MkCellCondition (NeighborsCountImpl s c)

data AdjacentsLvlImpl
  (lvl :: Nat)
type AdjacentsLvl lvl = MkNeighborhood (AdjacentsLvlImpl lvl)

data StateImpl
  (name :: Symbol)
  (idx  :: Nat)
type State n i = MkState (StateImpl n i)

data RuleImpl
  (name :: Symbol)
  (code :: Symbol)
  (nh   :: INeighborhood)
  (step :: CustomStep)
type Rule n c nh s = MkRule (RuleImpl n c nh s)

