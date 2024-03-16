{-# LANGUAGE DuplicateRecordFields #-}

module Cellular.App.Package.Rule where

import qualified Cellular.Language as L

-- | External rule & data package definitions: packaged rule

data State = State
  { name :: String
  , index :: Int
  }
  deriving (Read, Show, Eq, Ord)

data Transition = Transition
  { fromState :: Int
  , toState :: Int
  , cellCondition :: CellCondition
  }
  deriving (Read, Show, Eq, Ord)

data CellCondition
  = NeighborsCount
  { state :: Int         -- what state to count
  , quantities :: [Int]  -- how many cells of this state should be
  }                      --   to activate the condition
  deriving (Read, Show, Eq, Ord)

data Neighborhood = AdjacentsLvl
  { level :: Int
  }
  deriving (Read, Show, Eq, Ord)

data Rule = Rule
  { name :: String
  , code :: String
  , neighborhood :: Neighborhood
  , defaultState :: Int
  , transitionTable :: [Transition]
  }
  deriving (Read, Show, Eq, Ord)


toCustomStateTransition :: Transition -> L.CustomStateTransition
toCustomStateTransition (Transition from to (NeighborsCount st qs)) = let
    qs' = map fromIntegral qs
    cond' = L.NeighborsCount (fromIntegral st) qs'
  in L.StateTransition (fromIntegral from) (fromIntegral to) cond'

toDynamicRule :: Rule -> L.DynamicRule
toDynamicRule (Rule n c (AdjacentsLvl lvl) ds ts) = let
  nh' = L.AdjacentsLvl (fromIntegral lvl)
  ds' = L.DefState (fromIntegral ds)
  ts' = map toCustomStateTransition ts

  dynStep = L.DynamicStep ds' ts'
  in L.DynamicRule n c nh' dynStep

