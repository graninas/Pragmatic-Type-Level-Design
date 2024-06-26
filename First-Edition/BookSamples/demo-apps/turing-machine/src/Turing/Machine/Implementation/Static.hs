{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Static implementation of the Turing machine itself.
-- Addresses static type-level rules.

module Turing.Machine.Implementation.Static where

import Turing.Machine.Language
import Turing.Machine.Implementation.Common

import Lib.TypeSelector
import GHC.TypeLits
import Data.Proxy ( Proxy(..) )


--------------- Implementation ---------------


class RuleRunner (rule :: CustomRuleTL) where
  runRule
    :: Proxy rule
    -> Tape
    -> Either String Tape
  runRuleName
    :: Proxy rule
    -> String

class RuleRunnerImpl (rule :: CustomRuleTL) where
  runRule'
    :: Proxy rule
    -> CurrentStateIdx
    -> Tape
    -> Either String Tape


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

class StatesRunner (states :: [CustomStateTL]) where
  runStates
    :: Proxy states
    -> CurrentStateIdx
    -> Tape
    -> Result


-- | Transition conditions runner.

class ConditionsRunner (conds :: [CustomConditionTL]) where
  runConditions
    :: Proxy conds
    -> Tape
    -> Result


-- | Writing to tape action runner.

class WriteActionRunner (writeAct :: CustomWriteActionTL) where
  runWrite
    :: Proxy writeAct
    -> TapeSymbol
    -> Tape
    -> Tape


-- | Moving the tape head runner.

class MoveActionRunner (moveAct :: CustomMoveHeadActionTL) where
  runMove
    :: Proxy moveAct
    -> Tape
    -> Tape

--------------- Implementation ---------------

-- Initial runner.

-- No states provided - finishing.
instance
  ( KnownSymbol name
  ) =>
  RuleRunner ('Rule name stIdx '[]) where
  runRule _ tape = Right tape
  runRuleName _ = symbolVal $ Proxy @name

-- Starting the run with the init state idx as a current state
instance
  ( KnownNat initStateIdx
  , KnownSymbol name
  , RuleRunnerImpl ('Rule name initStateIdx (s ': ss))
  ) =>
  RuleRunner ('Rule name initStateIdx (s ': ss)) where
  runRule ruleProxy tape = let
    initStateIdx = fromIntegral $ natVal $ Proxy @initStateIdx
    in runRule' ruleProxy initStateIdx tape
  runRuleName _ = symbolVal $ Proxy @name


-- Actual step-by-step recursive rule runner.

instance
  ( StatesRunner states
  ) =>
  RuleRunnerImpl ('Rule n stIdx states) where
  runRule' ruleProxy curStateIdx tape1 = let
    res = runStates (Proxy @states) curStateIdx tape1
    in case res of
      FailedWith errMsg -> Left $ "[" <> show curStateIdx <> "] " <> errMsg
      Finished tape2 -> Right tape2
      Successful nextStateIdx tape2 ->
        runRule' ruleProxy nextStateIdx tape2


-- States runner

instance StatesRunner '[] where
  runStates _ _ _ = FailedWith "No matching states found"

instance StatesRunner ('FinishState idx n ': states) where
  runStates _ _ tape = Finished tape

instance
  ( StatesRunner states
  , ConditionsRunner conds
  , KnownNat idx
  ) =>
  StatesRunner ('State idx name conds ': states) where
    runStates _ curStateIdx tape
      | curStateIdx /= (fromIntegral $ natVal $ Proxy @idx)
      = runStates (Proxy @states) curStateIdx tape
    runStates _ _ tape = runConditions (Proxy @conds) tape


-- Conditions runner

instance ConditionsRunner '[] where
  runConditions _ _ = FailedWith "No matching conditions found"

instance
  ( ConditionsRunner conds
  , WriteActionRunner writeAct
  , MoveActionRunner moveAct
  , KnownNat nextStateIdx
  , KnownSymbol symb
  ) =>
  ConditionsRunner ('Match symb writeAct moveAct nextStateIdx ': conds) where
    runConditions _ tape1 = let
      curSymb = readTape tape1
      symb = symbolVal $ Proxy @symb
      nextStateIdx = fromIntegral $ natVal $ Proxy @nextStateIdx
      in case sameTapeSymbol curSymb symb of
        True -> let
          tape2 = runWrite (Proxy @writeAct) (toTapeSymbol symb) tape1
          tape3 = runMove (Proxy @moveAct) tape2
          in Successful nextStateIdx tape3
        False -> runConditions (Proxy @conds) tape1

instance
  ( ConditionsRunner conds
  , WriteActionRunner writeAct
  , MoveActionRunner moveAct
  , KnownNat nextStateIdx
  ) =>
  ConditionsRunner ('MatchAny writeAct moveAct nextStateIdx ': conds) where
    runConditions _ tape1 = let
      curSymb = readTape tape1
      nextStateIdx = fromIntegral $ natVal $ Proxy @nextStateIdx
      tape2 = runWrite (Proxy @writeAct) curSymb tape1
      tape3 = runMove (Proxy @moveAct) tape2
      in Successful nextStateIdx tape3

instance
  ( ConditionsRunner conds
  , WriteActionRunner writeAct
  , MoveActionRunner moveAct
  , KnownNat nextStateIdx
  ) =>
  ConditionsRunner ('MatchBlank writeAct moveAct nextStateIdx ': conds) where
    runConditions _ tape1 = let
      curSymb = readTape tape1
      in case isBlank curSymb of
        True -> let
          nextStateIdx = fromIntegral $ natVal $ Proxy @nextStateIdx
          tape2 = runWrite (Proxy @writeAct) curSymb tape1
          tape3 = runMove (Proxy @moveAct) tape2
          in Successful nextStateIdx tape3
        False -> runConditions (Proxy @conds) tape1

instance
  ( KnownSymbol msg
  ) =>
  ConditionsRunner ('FailWith msg ': conds) where
    runConditions _ _ = FailedWith $ symbolVal $ Proxy @msg

-- Write action runner

instance
  ( KnownSymbol symb
  ) =>
  WriteActionRunner ('Write symb) where
  runWrite _ _ tape = let
    symb = symbolVal $ Proxy @symb
    in writeTape tape symb

instance
  WriteActionRunner 'WriteMatched where
  runWrite _ matchedSymb tape = writeTape tape matchedSymb

instance
  WriteActionRunner 'WriteBlank where
  runWrite _ _ tape = writeTape @String tape ""

instance
  WriteActionRunner 'Skip where
  runWrite _ _ tape = tape


-- Move action runner

instance
  MoveActionRunner 'L where
  runMove _ tape = moveHeadLeft tape 1

instance
  ( KnownNat n
  ) =>
  MoveActionRunner ('Ln n) where
  runMove _ tape = let
    n = fromIntegral $ natVal $ Proxy @n
    in moveHeadLeft tape n

instance
  MoveActionRunner 'R where
  runMove _ tape = moveHeadRight tape 1

instance
  ( KnownNat n
  ) =>
  MoveActionRunner ('Rn n) where
  runMove _ tape = let
    n = fromIntegral $ natVal $ Proxy @n
    in moveHeadRight tape n

instance
  MoveActionRunner 'Stay where
  runMove _ tape = tape
