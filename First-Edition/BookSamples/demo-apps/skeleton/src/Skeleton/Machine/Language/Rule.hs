{-# LANGUAGE GADTs #-}

-- | Domain types that describe logic.
module Skeleton.Machine.Language.Rule where

import GHC.TypeLits


-- Turing Machine rules language

data CustomRule where
  Rule
    :: (ruleName :: Symbol)
    -> (initState :: Nat)
    -> [CustomState]
    -> CustomRule

data CustomState where
  State
    :: (stateIdx :: Nat)
    -> (stateName :: Symbol)
    -> [CustomCondition]
    -> CustomState
  FinishState
    :: (stateIdx :: Nat)
    -> (stateName :: Symbol)
    -> CustomState

data CustomCondition where

  -- | Matches specific symbol.
  Match
    :: (symbol :: Symbol)
    -> CustomWriteAction
    -> CustomMoveAction
    -> (stateIdx :: Nat)
    -> CustomCondition

  -- | Matches any symbol.
  AnyMatch
    :: CustomWriteAction
    -> CustomMoveAction
    -> (stateIdx :: Nat)
    -> CustomCondition

  -- | Matches any symbol and stops the evaluation.
  FailWith
    :: (msg :: Symbol)
    -> CustomCondition

data CustomWriteAction where
  Write :: (symbol :: Symbol) -> CustomWriteAction
  WriteMatched :: CustomWriteAction
  Skip :: CustomWriteAction           -- Same as WriteMatched

data CustomMoveAction where
  Ln :: (steps :: Nat) -> CustomMoveAction
  L  :: CustomMoveAction              -- Same as L 1
  Rn :: (steps :: Nat) -> CustomMoveAction
  R  :: CustomMoveAction              -- Same as R 1
  Stay :: CustomMoveAction


