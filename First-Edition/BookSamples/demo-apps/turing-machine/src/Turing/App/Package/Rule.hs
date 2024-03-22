{-# LANGUAGE DuplicateRecordFields #-}

module Turing.App.Package.Rule where

-- | External rule & data package definitions: packaged rule.

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

data Rule = Rule
  { name :: String
  , initState :: Int
  , states :: [State]
  }
  deriving (Read, Show, Eq, Ord)


-- toCustomStateTransition :: Transition -> L.CustomStateTransition
-- toCustomStateTransition (Transition from to (NeighborsCount st qs)) = let
--     qs' = map fromIntegral qs
--     cond' = L.NeighborsCount (fromIntegral st) qs'
--   in L.StateTransition (fromIntegral from) (fromIntegral to) cond'

-- toDynamicRule :: Rule -> L.DynamicRule
-- toDynamicRule (Rule n c (AdjacentsLvl lvl) ds ts) = let
--   nh' = L.AdjacentsLvl (fromIntegral lvl)
--   ds' = L.DefState (fromIntegral ds)
--   ts' = map toCustomStateTransition ts

--   dynStep = L.DynamicStep ds' ts'
--   in L.DynamicRule n c nh' dynStep

