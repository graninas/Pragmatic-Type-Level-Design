{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Turing.App.Package.Rule where

import qualified Turing.Machine.Language as L

import Lib.TypeSelector

-- | External rule & data package definitions: packaged rule.

data Rule = Rule
  { name :: String
  , initState :: Int
  , states :: [State]
  }
  deriving (Read, Show, Eq, Ord)

data State
  = State
  { index :: Int
  , name :: String
  , conditions :: [Condition]
  }
  | FinishState
  { index :: Int
  , name :: String
  }
  deriving (Read, Show, Eq, Ord)

data Condition
  = Match
    { symbol :: Char
    , writeAction :: WriteAction
    , moveAction :: MoveHeadAction
    , stateIdx :: Int
    }
  | MatchAny
    { writeAction :: WriteAction
    , moveAction :: MoveHeadAction
    , stateIdx :: Int
    }
  | MatchBlank
    { writeAction :: WriteAction
    , moveAction :: MoveHeadAction
    , stateIdx :: Int
    }
  | FailWith
    { failMessage :: String
    }
  deriving (Read, Show, Eq, Ord)

data WriteAction
  = Write
    { symbol :: Char
    }
  | WriteMatched
  | Skip
  deriving (Read, Show, Eq, Ord)

data MoveHeadAction
  = Ln
    { steps :: Int
    }
  | L              -- Same as L 1
  | Rn
    { steps :: Int
    }
  | R              -- Same as R 1
  | Stay
  deriving (Read, Show, Eq, Ord)

toDynamicWriteAction :: WriteAction -> L.CustomWriteAction 'ValueLevel
toDynamicWriteAction (Write ch) = L.Write ch
toDynamicWriteAction WriteMatched = L.WriteMatched
toDynamicWriteAction Skip = L.Skip

toDynamicMoveAction :: MoveHeadAction -> L.CustomMoveHeadAction 'ValueLevel
toDynamicMoveAction (Ln s) = L.Ln s
toDynamicMoveAction (Rn s) = L.Rn s
toDynamicMoveAction L  = L.L
toDynamicMoveAction R = L.R
toDynamicMoveAction Stay = L.Stay

toDynamicCondition :: Condition -> L.CustomCondition 'ValueLevel
toDynamicCondition (Match ch wAct mAct idx) = let
  wAct' = toDynamicWriteAction wAct
  mAct' = toDynamicMoveAction mAct
  in L.Match ch wAct' mAct' idx
toDynamicCondition (MatchAny wAct mAct idx) = let
  wAct' = toDynamicWriteAction wAct
  mAct' = toDynamicMoveAction mAct
  in L.MatchAny wAct' mAct' idx
toDynamicCondition (MatchBlank wAct mAct idx) = let
  wAct' = toDynamicWriteAction wAct
  mAct' = toDynamicMoveAction mAct
  in L.MatchBlank wAct' mAct' idx
toDynamicCondition (FailWith msg) =
  L.FailWith msg

toDynamicState :: State -> L.CustomState 'ValueLevel
toDynamicState (State idx n conds) =
  L.State idx n (map toDynamicCondition conds)
toDynamicState (FinishState idx n) =
  L.FinishState idx n

toDynamicRule :: Rule -> L.CustomRule 'ValueLevel
toDynamicRule (Rule n sIdx ss) =
  L.Rule n sIdx (map toDynamicState ss)

