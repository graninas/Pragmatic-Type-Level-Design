{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- | Domain types that describe logic.
module Turing.Machine.Language.Rule where

import GHC.TypeLits


-- Turing Machine rules language

-- | Rule for the Turing Machine.
data CustomRule
  = Rule
    { crRuleName :: Symbol
    , crInitState :: Nat
    , crStates :: [CustomState]
    }

-- | State and state transition.
data CustomState
  = State
    { csStateIdx :: Nat
    , csStateName :: Symbol
    , csConditions :: [CustomCondition]
    }
  | FinishState
    { csStateIdx :: Nat
    , csStateName :: Symbol
    }

-- | Matching tape symbols type for conditional state transition.
data CustomCondition

  -- | Matches specific symbol.
  = Match
    { ccSymbol :: Symbol
    , ccWriteAction :: CustomWriteAction
    , ccMoveAction :: CustomMoveAction
    , ccStateIdx :: Nat
    }

  -- | Matches any symbol.
  | AnyMatch
    { ccWriteAction :: CustomWriteAction
    , ccMoveAction :: CustomMoveAction
    , ccStateIdx :: Nat
    }

  -- | Matches any symbol and stops the evaluation.
  | FailWith
    { ccFailMessage :: Symbol
    }

-- | Tape writing action.
data CustomWriteAction
  = Write
    { cwaSymbol :: Symbol
    }
  | WriteMatched
  | Skip           -- Same as WriteMatched

-- | Tape head moving action.
data CustomMoveAction
  = Ln
    { smaSteps :: Nat
    }
  | L              -- Same as L 1
  | Rn
    { cmaSteps :: Nat
    }
  | R              -- Same as R 1
  | Stay


