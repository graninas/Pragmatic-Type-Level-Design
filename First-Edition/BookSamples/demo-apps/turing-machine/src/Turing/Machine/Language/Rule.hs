{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- | Domain types that describe logic.
module Turing.Machine.Language.Rule where

import Lib.TypeSelector

import GHC.TypeLits


-- Turing Machine rules language.

-- | Rule for the Turing Machine.
data CustomRule (lvl :: Level)
  = Rule
    { crRuleName :: StringType lvl
    , crInitState :: IntType lvl
    , crStates :: [CustomState lvl]
    }
  | DynamicRule

-- | State and state transition.
data CustomState (lvl :: Level)
  = State
    { csStateIdx :: IntType lvl
    , csStateName :: StringType lvl
    , csConditions :: [CustomCondition lvl]
    }
  | FinishState
    { csStateIdx :: IntType lvl
    , csStateName :: StringType lvl
    }

-- | Matching tape symbols type for conditional state transition.
data CustomCondition (lvl :: Level)
  -- | Matches specific symbol.
  = Match
    { ccSymbol :: CharType lvl
    , ccWriteAction :: CustomWriteAction lvl
    , ccMoveAction :: CustomMoveHeadAction lvl
    , ccStateIdx :: IntType lvl
    }

  -- | Matches any symbol.
  | MatchAny
    { ccWriteAction :: CustomWriteAction lvl
    , ccMoveAction :: CustomMoveHeadAction lvl
    , ccStateIdx :: IntType lvl
    }

  -- | Matches empty cell.
  | MatchBlank
    { ccWriteAction :: CustomWriteAction lvl
    , ccMoveAction :: CustomMoveHeadAction lvl
    , ccStateIdx :: IntType lvl
    }

  -- | Matches any symbol and stops the evaluation.
  | FailWith
    { ccFailMessage :: StringType lvl
    }

-- | Tape writing action.
data CustomWriteAction (lvl :: Level)
  = Write
    { cwaSymbol :: CharType lvl
    }
  | WriteMatched
  | Skip           -- Same as WriteMatched

-- | Tape head moving action.
data CustomMoveHeadAction (lvl :: Level)
  = Ln
    { smaSteps :: IntType lvl
    }
  | L              -- Same as L 1
  | Rn
    { cmaSteps :: IntType lvl
    }
  | R              -- Same as R 1
  | Stay


