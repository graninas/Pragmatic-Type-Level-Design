{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- | Domain types that describe logic.
module Turing.Machine.Language.Rule where

import GHC.TypeLits


-- Turing Machine rules language.

-- | Init state.

newtype InitState = InitState Nat
type RuleName = Symbol
type StateName = Symbol

-- | Rule for the Turing Machine.
data CustomRule
  = Rule
    { crRuleName :: RuleName
    , crInitState :: InitState
    , crStates :: [CustomState]
    }

-- | State and state transition.
data CustomState
  = State
    { csStateIdx :: Nat
    , csStateName :: StateName
    , csConditions :: [CustomCondition]
    }
  | FinishState
    { csStateIdx :: Nat
    , csStateName :: StateName
    }

-- | Matching tape symbols type for conditional state transition.
data CustomCondition
  -- | Matches specific symbol.
  = Match
    { ccSymbol :: Symbol
    , ccWriteAction :: CustomWriteAction
    , ccMoveAction :: CustomMoveHeadAction
    , ccStateIdx :: Nat
    }

  -- | Matches any symbol.
  | MatchAny
    { ccWriteAction :: CustomWriteAction
    , ccMoveAction :: CustomMoveHeadAction
    , ccStateIdx :: Nat
    }

  -- | Matches empty cell.
  | MatchBlank
    { ccWriteAction :: CustomWriteAction
    , ccMoveAction :: CustomMoveHeadAction
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
data CustomMoveHeadAction
  = Ln
    { smaSteps :: Nat
    }
  | L              -- Same as L 1
  | Rn
    { cmaSteps :: Nat
    }
  | R              -- Same as R 1
  | Stay


