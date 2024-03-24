{-# LANGUAGE DataKinds #-}
module Turing.App.App where

import Turing.App.Storage
import Turing.App.Commands
import Turing.App.State
import Turing.App.Action ( AppAction, continue, continueWithMsg, finish )
import qualified Turing.App.Package.Rule as R
import Turing.Machine.Interface
import Turing.Machine.Language
import Turing.Machine.Language.Materialization
import Turing.Assets.Rules
import Turing.Assets.Tapes

import Lib.TypeSelector
import qualified Data.Map as Map
import Data.Proxy ( Proxy(..) )
import Data.IORef ( IORef, readIORef, writeIORef )
import Data.Traversable (for)
import Data.List (intercalate)
import Control.Exception ( SomeException, try )
import System.Directory
import Text.Read (readMaybe)


runCommand :: Command -> AppState -> IO AppAction
runCommand Help _ = printCommandsHelp >> continue
runCommand Quit _ = finish
runCommand Tapes st = processListTapes st
runCommand (NewTape str) st = processNewTape st str
runCommand (LoadTape str) st = processLoadTape st str
runCommand (PrintTape tapeIdx) st = processPrintTape st tapeIdx
runCommand Rules st = processListRules st
runCommand LoadPredefTapes st = processLoadPredefTapes st
runCommand (LoadRule str) st = processLoadRule st str
runCommand LoadPredefRules st = processLoadPredefRules st
runCommand Materialize st = processMaterialize st
runCommand (Run ruleIdx tapeIdx) st = processRun st ruleIdx tapeIdx


-- App interface

processListRules :: AppState -> IO AppAction
processListRules (AppState rulesRef _) = do
  rules <- readIORef rulesRef
  putStrLn "\nSupported rules ([idx] name):"
  mapM_ f $ Map.toList rules
  continue
  where
    f (ruleIdx, ri) = do
      let ruleKind = if isStatic ri then "static" else "dynamic"
      putStrLn ("[" <> show ruleIdx <> "] (" <>
        ruleKind <> ") " <> getName ri)

printTape' :: (TapeIndex, Tape) -> IO ()
printTape' (idx, tape) = do
  putStrLn $ "[" <> show idx <> "] \"" <> printTape tape <> "\""

processListTapes :: AppState -> IO AppAction
processListTapes (AppState _ tapesRef) = do
  tapes <- readIORef tapesRef
  putStrLn ("\nTapes available: " <> show (Map.size tapes))
  let ts = Map.toAscList tapes
  mapM_ printTape' ts
  continue

processNewTape :: AppState -> String -> IO AppAction
processNewTape st str = do
  idx <- addTape st $ initTape str
  continueWithMsg $ "Tape idx: " <> show idx

processLoadTape :: AppState -> String -> IO AppAction
processLoadTape appState@(AppState _ tapesRef) tapePath = do
  tapeStr <- readFile tapePath
  let tape = initTape tapeStr
  tapes <- readIORef tapesRef
  let idx = Map.size tapes
  let tapes' = Map.insert idx tape tapes
  writeIORef tapesRef tapes'
  continueWithMsg $ "Tape loaded: " <> show idx

processPrintTape :: AppState -> TapeIndex -> IO AppAction
processPrintTape (AppState _ tapesRef) tapeIdx = do
  tapes <- readIORef tapesRef
  case Map.lookup tapeIdx tapes of
    Nothing -> continueWithMsg "Tape doesn't exist."
    Just tape -> do
      printTape' (tapeIdx, tape)
      continue

processLoadRule :: AppState -> String -> IO AppAction
processLoadRule appState@(AppState rulesRef _) rulePath = do
  ruleStr <- readFile rulePath
  case readMaybe ruleStr of
    Nothing -> continueWithMsg "Failed to parse the rule."
    -- TODO: validation
    Just (rule :: R.Rule) -> do
      let rule' = R.toDynamicRule rule
      let ri = DynRI rule'
      rules <- readIORef rulesRef
      let idx = Map.size rules
      let rules' = Map.insert idx ri rules
      writeIORef rulesRef rules'
      continueWithMsg $ "Rule loaded: ["
        <> show idx <> "] (dynamic) "
        <> getName ri

processLoadPredefRules :: AppState -> IO AppAction
processLoadPredefRules appState@(AppState rulesRef _) = do
  mapM_ f supportedRules
  continueWithMsg "Predefined rules loaded."
  where
    f (_, ri) = do
      let ruleKind = if isStatic ri then "static" else "dynamic"
      rules <- readIORef rulesRef
      let idx = Map.size rules
      let rules' = Map.insert idx ri rules
      writeIORef rulesRef rules'
      putStrLn $ "Rule loaded: [" <> show idx <> "] ("
        <> ruleKind <> ") "
        <> getName ri

processLoadPredefTapes :: AppState -> IO AppAction
processLoadPredefTapes appState@(AppState _ tapesRef) = do
  mapM_ f predefinedTapes
  continueWithMsg "Predefined tapes loaded."
  where
    f (n, tape) = do
      tapes <- readIORef tapesRef
      let idx = Map.size tapes
      let tapes' = Map.insert idx tape tapes
      writeIORef tapesRef tapes'
      putStrLn $ "Tape loaded: [" <> show idx <> "] " <> n

processMaterialize
  :: AppState
  -> IO AppAction
processMaterialize (AppState rulesRef _) = do
  rules <- readIORef rulesRef
  mapM_ f $ Map.toList rules
  continue
  where
    f (idx, (DynRI _)) = pure ()
    f (idx, r@(RI proxy)) = do
      let rule = mat () proxy
      rules' <- readIORef rulesRef
      writeIORef rulesRef $ Map.insert idx (DynRI rule) rules'
      putStrLn $ "Materialized: [" <> show idx
        <> "] " <> getName r

processRun :: AppState -> RuleIndex -> TapeIndex -> IO AppAction
processRun (AppState rulesRef tapesRef) ruleIdx tapeIdx = do
  rules <- readIORef rulesRef
  tapes <- readIORef tapesRef
  case (Map.lookup ruleIdx rules, Map.lookup tapeIdx tapes) of
    (Nothing, _) -> continueWithMsg "Rule doesn't exist."
    (_, Nothing) -> continueWithMsg "Tape doesn't exist."
    (Just (RI proxy), Just tape1) -> do
      let eTape2 = run () proxy tape1
      case eTape2 of
        Left err -> continueWithMsg err
        Right tape2 -> do
          let tapes' = Map.insert tapeIdx tape2 tapes
          writeIORef tapesRef tapes'
          printTape' (tapeIdx, tape2)
          continue
    (Just (DynRI rule), Just tape1) -> do
      let eTape2 = run rule (Proxy @DynamicRule) tape1
      case eTape2 of
        Left err -> continueWithMsg err
        Right tape2 -> do
          let tapes' = Map.insert tapeIdx tape2 tapes
          writeIORef tapesRef tapes'
          printTape' (tapeIdx, tape2)
          continue
