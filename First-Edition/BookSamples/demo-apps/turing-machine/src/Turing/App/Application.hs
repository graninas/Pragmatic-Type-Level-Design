{-# LANGUAGE DataKinds #-}
module Turing.App.Application where

import Turing.App.Storage
import qualified Turing.App.Commands as Cmds
import Turing.App.InterfacingSwitch
import Turing.App.State
import Turing.App.Action ( AppAction, continue, continueWithMsg, finish )
import qualified Turing.App.Package.Rule as R
import Turing.Machine.Interface.TypeClass
import Turing.Machine.Interface.FreeMonad
import Turing.Machine.Language
import Turing.Machine.Language.Materialization
import Turing.Machine.Implementation.FreeMonad.Static
import Turing.Machine.Implementation.FreeMonad.Dynamic
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


-- | Running a command.
-- Interface switch defines what interfacing mechanism
-- should be used for demo.
runCommand
  :: InterfacingSwitch
  -> Cmds.Command
  -> AppState
  -> IO AppAction
runCommand _ Cmds.Help _ = Cmds.printCommandsHelp >> continue
runCommand _ Cmds.Quit _ = finish
runCommand _ Cmds.Tapes st = processListTapes st
runCommand _ (Cmds.NewTape str) st = processNewTape st str
runCommand _ (Cmds.LoadTape str) st = processLoadTape st str
runCommand _ (Cmds.PrintTape tapeIdx) st = processPrintTape st tapeIdx
runCommand _ Cmds.LoadPredefTapes st = processLoadPredefTapes st
runCommand _ Cmds.Rules st = processListRules st
runCommand iSwitch (Cmds.LoadRule str) st = processLoadRule iSwitch st str
runCommand _ Cmds.LoadPredefRules st = processLoadPredefRules st
runCommand iSwitch Cmds.Materialize st = processMaterialize iSwitch st
runCommand _ (Cmds.Run ruleIdx tapeIdx) st = processRun st ruleIdx tapeIdx


-- App interface

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

processListRules :: AppState -> IO AppAction
processListRules (AppState rulesRef _) = do
  rules <- readIORef rulesRef
  putStrLn "\nSupported rules ([idx] name):"
  mapM_ f $ Map.toList rules
  continue
  where
    f (ruleIdx, (ri, ruleKind)) = do
      putStrLn ("[" <> show ruleIdx <> "] (" <>
        ruleKind <> ") " <> getName ri)

makeDynRuleImpl
  :: InterfacingSwitch
  -> String
  -> CustomRuleVL
  -> (RuleImpl, String)
makeDynRuleImpl TypeClass kind rule =
  (TypeClassDynRI rule, kind <> ", type class")
makeDynRuleImpl FreeMonad kind rule =
  (FreeMonadRI (dynamicRuleInterpreter rule), kind <> ", free monad")

processLoadRule
  :: InterfacingSwitch -> AppState -> String -> IO AppAction
processLoadRule iSwitch appState@(AppState rulesRef _) rulePath = do
  ruleStr <- readFile rulePath
  case readMaybe ruleStr of
    Nothing -> continueWithMsg "Failed to parse the rule."
    -- TODO: validation
    Just (rule :: R.Rule) -> do
      let (ri, ruleKind) = makeDynRuleImpl iSwitch "from file/dynamic" (R.toDynamicRule rule)
      rules <- readIORef rulesRef
      let idx = Map.size rules
      let rules' = Map.insert idx (ri, ruleKind) rules
      writeIORef rulesRef rules'
      continueWithMsg $ "Rule loaded: ["
            <> show idx <> "] " <> ruleKind <> " "
            <> getName ri

processLoadPredefRules
  :: AppState -> IO AppAction
processLoadPredefRules appState@(AppState rulesRef _) = do
  mapM_ f supportedRules
  continueWithMsg "Predefined rules loaded."
  where
    f (_, (ri, ruleKind)) = do
      rules <- readIORef rulesRef
      let idx = Map.size rules
      let rules' = Map.insert idx (ri, ruleKind) rules
      writeIORef rulesRef rules'
      putStrLn $ "Rule loaded: [" <> show idx <> "] ("
        <> ruleKind <> ") "
        <> getName ri

processMaterialize
  :: InterfacingSwitch
  -> AppState
  -> IO AppAction
processMaterialize iSwitch (AppState rulesRef _) = do
  rules <- readIORef rulesRef
  mapM_ f $ Map.toList rules
  continue
  where
    f (idx, (TypeClassDynRI _, _)) = pure ()
    f (idx, (FreeMonadRI _, _)) = pure ()
    f (idx, r@(TypeClassRI proxy, _)) = do
      let rule = mat () proxy
      let (ri, ruleKind) = makeDynRuleImpl iSwitch "materialized/dynamic" rule
      rules' <- readIORef rulesRef
      writeIORef rulesRef $ Map.insert idx (ri, ruleKind) rules'
      putStrLn $ "Materialized: [" <> show idx
        <> "] " <> getName ri

processRun :: AppState -> RuleIndex -> TapeIndex -> IO AppAction
processRun (AppState rulesRef tapesRef) ruleIdx tapeIdx = do
  rules <- readIORef rulesRef
  tapes <- readIORef tapesRef
  case (Map.lookup ruleIdx rules, Map.lookup tapeIdx tapes) of
    (Nothing, _) -> continueWithMsg "Rule doesn't exist."
    (_, Nothing) -> continueWithMsg "Tape doesn't exist."
    (Just (ri@(TypeClassRI proxy), ruleKind), Just tape1) -> do
      putStrLn $ "Running: " <> getName ri <> " (" <> ruleKind <> ")"
      let eTape2 = run () proxy tape1
      case eTape2 of
        Left err -> continueWithMsg err
        Right tape2 -> do
          let tapes' = Map.insert tapeIdx tape2 tapes
          writeIORef tapesRef tapes'
          printTape' (tapeIdx, tape2)
          continue
    (Just (ri@(TypeClassDynRI rule), ruleKind), Just tape1) -> do
      putStrLn $ "Running: " <> getName ri <> " (" <> ruleKind <> ")"
      let eTape2 = run rule (Proxy @DynamicRule) tape1
      case eTape2 of
        Left err -> continueWithMsg err
        Right tape2 -> do
          let tapes' = Map.insert tapeIdx tape2 tapes
          writeIORef tapesRef tapes'
          printTape' (tapeIdx, tape2)
          continue
    (Just (ri@(FreeMonadRI ruleInterpreter), ruleKind), Just tape1) -> do
      putStrLn $ "Running: " <> getName ri <> " (" <> ruleKind <> ")"
      let eTape2 = ruleInterpreter (runFM tape1)
      case eTape2 of
        Left err -> continueWithMsg err
        Right tape2 -> do
          let tapes' = Map.insert tapeIdx tape2 tapes
          writeIORef tapesRef tapes'
          printTape' (tapeIdx, tape2)
          continue
