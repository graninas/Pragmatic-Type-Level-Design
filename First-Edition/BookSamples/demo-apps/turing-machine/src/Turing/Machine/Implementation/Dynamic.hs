{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Dynamic implementation of the Turing machine itself.
-- Addresses dynamic value-level rules.

module Turing.Machine.Implementation.Dynamic where

import Turing.Machine.Language
import Turing.Machine.Implementation.Common

import Lib.TypeSelector
import GHC.TypeLits
import Data.Proxy ( Proxy(..) )



--------------- Implementation ---------------

runDynamicRule
  :: CustomRuleVL
  -> Tape
  -> Either String Tape
runDynamicRule DynamicRuleTag _ = error "DynamicRuleTag placeholder can't be run."
runDynamicRule (Rule _ _ []) tape = Right tape
runDynamicRule r@(Rule _ initStateIdx _) tape =
  runDynamicRule' r initStateIdx tape

runDynamicRule'
  :: CustomRuleVL
  -> Int
  -> Tape
  -> Either String Tape
runDynamicRule' DynamicRuleTag _ _ = error "DynamicRuleTag placeholder can't be run."
runDynamicRule' r@(Rule _ stIdx states) curStateIdx tape1 = let
  res = runDynamicStates states curStateIdx tape1
  in case res of
    FailedWith errMsg -> Left $ "[" <> show curStateIdx <> "] " <> errMsg
    Finished tape2 -> Right tape2
    Successful nextStateIdx tape2 ->
      runDynamicRule' r nextStateIdx tape2


-- States runner

runDynamicStates
  :: [CustomStateVL]
  -> Int
  -> Tape
  -> Result
runDynamicStates [] _ _ = FailedWith "No matching states found"
runDynamicStates (FinishState _ _ : _) _ tape = Finished tape
runDynamicStates (State idx _ conds : states) curStateIdx tape
  | curStateIdx /= idx
  = runDynamicStates states curStateIdx tape
runDynamicStates (State _ _ conds : _) _ tape
  = runDynamicConditions conds tape


-- Conditions runner

runDynamicConditions :: [CustomConditionVL] -> Tape -> Result
runDynamicConditions [] _ = FailedWith "No matching conditions found"
runDynamicConditions (Match symb writeAct moveAct nextStateIdx : conds) tape1 = let
  curSymb = readTape tape1
  in case sameTapeSymbol curSymb symb of
    True -> let
      tape2 = runDynamicWrite writeAct (toTapeSymbol symb) tape1
      tape3 = runDynamicMove moveAct tape2
      in Successful nextStateIdx tape3
    False -> runDynamicConditions conds tape1

runDynamicConditions (MatchAny writeAct moveAct nextStateIdx : _) tape1 = let
  curSymb = readTape tape1
  tape2 = runDynamicWrite writeAct curSymb tape1
  tape3 = runDynamicMove moveAct tape2
  in Successful nextStateIdx tape3

runDynamicConditions (MatchBlank writeAct moveAct nextStateIdx : conds) tape1 = let
  curSymb = readTape tape1
  in case isBlank curSymb of
    True -> let
      tape2 = runDynamicWrite writeAct curSymb tape1
      tape3 = runDynamicMove moveAct tape2
      in Successful nextStateIdx tape3
    False -> runDynamicConditions conds tape1

runDynamicConditions (FailWith msg : _) _ = FailedWith msg


-- Write action runner

runDynamicWrite
  :: CustomWriteActionVL
  -> TapeSymbol
  -> Tape
  -> Tape
runDynamicWrite (Write symb) _ tape = writeTape tape symb
runDynamicWrite WriteMatched matchedSymb tape =
  writeTape tape matchedSymb
runDynamicWrite WriteBlank _ tape = writeTape @String tape ""
runDynamicWrite Skip _ tape = tape


-- Move action runner

runDynamicMove
  :: CustomMoveHeadActionVL
  -> Tape
  -> Tape
runDynamicMove L tape = moveHeadLeft tape 1
runDynamicMove (Ln n) tape = moveHeadLeft tape n
runDynamicMove R tape = moveHeadRight tape 1
runDynamicMove (Rn n) tape = moveHeadRight tape n
runDynamicMove Stay tape = tape
