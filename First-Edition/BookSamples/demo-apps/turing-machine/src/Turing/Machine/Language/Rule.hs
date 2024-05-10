{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- | Domain types that describe logic.
module Turing.Machine.Language.Rule where

import Lib.TypeSelector

import GHC.TypeLits


-- Turing Machine rules language.

-- | Rule for the Turing Machine.
-- Supports both dynamic and static models
-- with Granular Type Selector pattern.
data CustomRule (lvl :: Level)
  = Rule
    { crRuleName :: StringType lvl
    , crInitState :: IntType lvl
    , crStates :: [CustomState lvl]
    }
  | DynamicRuleTag

-- | Tag for dynamic rule machinery.
type DynamicRule = 'DynamicRuleTag @'ValueLevel

-- | State and state transition.
-- Supports both dynamic and static models
-- with Granular Type Selector pattern.
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
-- Supports both dynamic and static models
-- with Granular Type Selector pattern.
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
-- Supports both dynamic and static models
-- with Granular Type Selector pattern.
data CustomWriteAction (lvl :: Level)
  = Write
    { cwaSymbol :: CharType lvl
    }
  | WriteBlank
  | WriteMatched
  | Skip           -- Same as WriteMatched

-- | Tape head moving action.
-- Supports both dynamic and static models
-- with Granular Type Selector pattern.
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


-- Short aliaces.

type CustomMoveHeadActionTL = CustomMoveHeadAction 'TypeLevel
type CustomMoveHeadActionVL = CustomMoveHeadAction 'ValueLevel

type CustomWriteActionTL = CustomWriteAction 'TypeLevel
type CustomWriteActionVL = CustomWriteAction 'ValueLevel

type CustomConditionTL = CustomCondition 'TypeLevel
type CustomConditionVL = CustomCondition 'ValueLevel

type CustomStateTL = CustomState 'TypeLevel
type CustomStateVL = CustomState 'ValueLevel

type CustomRuleTL = CustomRule 'TypeLevel
type CustomRuleVL = CustomRule 'ValueLevel

