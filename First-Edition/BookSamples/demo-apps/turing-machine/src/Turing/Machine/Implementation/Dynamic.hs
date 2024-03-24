{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Turing.Machine.Implementation.Dynamic where

import Turing.Machine.Language
import Turing.Machine.Interface
import Turing.Machine.Implementation.Common

import Lib.TypeSelector
import GHC.TypeLits
import Data.Proxy ( Proxy(..) )


instance
  IMachine (CustomRule 'ValueLevel) DynamicRule where
  run rule _ = runDynamicRule rule
  name (Rule n _ _) _ = n

--------------- Implementation ---------------

runDynamicRule
  :: CustomRule 'ValueLevel
  -> Tape
  -> Either String Tape
runDynamicRule (Rule _ _ []) tape = Right tape
runDynamicRule r@(Rule _ initStateIdx _) tape =
  runDynamicRule' r initStateIdx tape

runDynamicRule'
  :: CustomRule 'ValueLevel
  -> Int
  -> Tape
  -> Either String Tape
runDynamicRule' r@(Rule _ stIdx states) curStateIdx tape1 = let
  res = runDynamicStates states curStateIdx tape1
  in case res of
    FailedWith errMsg -> Left $ "[" <> show curStateIdx <> "] " <> errMsg
    Finished tape2 -> Right tape2
    Successful nextStateIdx tape2 ->
      runDynamicRule' r nextStateIdx tape2


-- States runner

runDynamicStates
  :: [CustomState 'ValueLevel]
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

runDynamicConditions :: [CustomCondition 'ValueLevel] -> Tape -> Result
runDynamicConditions [] _ = FailedWith "No matching conditions found"
runDynamicConditions (Match symb writeAct moveAct nextStateIdx : conds) tape1 = let
  curSymb = readTape tape1
  in case sameTapeSymbol curSymb symb of
    True -> let
      tape2 = runDynamicWrite writeAct (toTapeSymbol symb) tape1
      tape3 = runDynamicMove moveAct tape2
      in Successful nextStateIdx tape3
    False -> runDynamicConditions conds tape1

runDynamicConditions (MatchAny writeAct moveAct nextStateIdx : conds) tape1 = let
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
  :: CustomWriteAction 'ValueLevel
  -> TapeSymbol
  -> Tape
  -> Tape
runDynamicWrite (Write symb) _ tape = writeTape tape symb
runDynamicWrite WriteMatched matchedSymb tape = writeTape tape matchedSymb
runDynamicWrite Skip _ tape = tape


-- Move action runner

runDynamicMove
  :: CustomMoveHeadAction 'ValueLevel
  -> Tape
  -> Tape
runDynamicMove L tape = moveHeadLeft tape 1
runDynamicMove (Ln n) tape = moveHeadLeft tape n
runDynamicMove R tape = moveHeadRight tape 1
runDynamicMove (Rn n) tape = moveHeadRight tape n
runDynamicMove Stay tape = tape
