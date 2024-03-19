{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- | Domain types that describe logic.
module Skeleton.Machine.Language.Rule where

import GHC.TypeLits


-- Turing Machine rules language

data CustomRule
  = Rule
    { crRuleName :: Symbol
    , crInitState :: Nat
    , crStates :: [CustomState]
    }

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

data CustomWriteAction
  = Write
    { cwaSymbol :: Symbol
    }
  | WriteMatched
  | Skip           -- Same as WriteMatched

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


