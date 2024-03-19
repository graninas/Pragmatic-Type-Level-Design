{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Turing.Machine.Runner.Static where

import Turing.Machine.Language

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Type.Equality


-- TODO: MaxSteps can be type-level
type MaxSteps = Int
type StateIdx = Int
type CurrentStateIdx = StateIdx

class RuleRunner (rule :: CustomRule) where
  runRule
    :: Proxy rule
    -> Tape
    -> Tape

class RuleRunnerImpl (rule :: CustomRule) where
  runRule'
    :: Proxy rule
    -> CurrentStateIdx
    -> Tape
    -> Tape


data StatesResult
  = FailedWith String
  | Finished Tape
  | Successful StateIdx Tape

-- | States recursive runner.

-- | Applies the state if it matches the current state.
-- N.B. This is the overlapping instances workaround.
--   We can't have 2 instances of RuleRunner:
-- instance
--   ( (curState == stIdx) ~ 'True
--   ) =>
--   RuleRunner ...
--
-- instance
--   ( (curState == stIdx) ~ 'False
--   ) =>
--   RuleRunner ...
--
-- These instances overlap.
-- Moving the matching of the states to the value level solves this.

class StatesRunner (states :: [CustomState]) where
  runStates
    :: Proxy states
    -> CurrentStateIdx
    -> Tape
    -> StatesResult

-- Initial runner.

-- No states provided - finishing.
instance
  RuleRunner ('Rule n stIdx '[]) where
  runRule _ tape = tape

-- Starting the run with the init state idx as a current state
instance
  ( KnownNat initStateIdx
  , RuleRunnerImpl ('Rule name initStateIdx (state ': states))
  ) =>
  RuleRunner ('Rule name initStateIdx (state ': states)) where
  runRule ruleProxy tape = let
    initStateIdx = fromIntegral $ natVal $ Proxy @initStateIdx
    in runRule' ruleProxy initStateIdx tape

-- Actual runner


instance
  ( StatesRunner states
  ) =>
  RuleRunnerImpl ('Rule n stIdx states) where
  runRule' ruleProxy curStateIdx tape1 = let
    res = runStates (Proxy @states) curStateIdx tape1
    in case res of
      FailedWith errMsg -> error errMsg       -- TODO: proper error
      Finished tape2 -> tape2
      Successful nextStateIdx tape2 ->
        runRule' ruleProxy nextStateIdx tape2

instance StatesRunner '[] where
  runStates _ _ tape = FailedWith "No matching states found"

instance StatesRunner ('FinishState idx n ': states) where
  runStates _ _ tape = Finished tape

instance
  ( StatesRunner states
  , KnownNat idx
  ) =>
  StatesRunner ('State idx name conds ': states) where
    runStates _ curStateIdx tape
      | curStateIdx /= (fromIntegral $ natVal $ Proxy @idx)
      = runStates (Proxy @states) curStateIdx tape
    runStates _ _ tape = Finished tape   -- TODO

