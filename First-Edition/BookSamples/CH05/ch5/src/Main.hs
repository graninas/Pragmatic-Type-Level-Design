{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE DataKinds #-}

module Main where

import qualified Cellular.App.App as EApp
import Cellular.App.State (AppState(..), addRule, createAppState)
import Cellular.App.Action (AppAction (..), continue, finish, continueWithMsg)
import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.Seeds
import Cellular.Assets.Automata.Replicator
import Cellular.Assets.Automata.Rules

import qualified Data.Map as Map
import Data.IORef (IORef, newIORef)
import Data.Proxy
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)


import Cellular.Automaton
import Cellular.Language.Automaton
import Cellular.App.Storage.Rules
import qualified Cellular.App.Package.Rule as Package


printHelp :: IO AppAction
printHelp = do
  putStrLn "\nOptional arg: path_to_external_rule"

  putStrLn "\nCommands:"
  putStrLn "help   - this help message"
  putStrLn "quit   - exit"
  putStrLn "rules  - list rules"
  putStrLn "load   - load a world"
  putStrLn "predef - load predefined worlds"
  putStrLn "worlds - list active worlds"
  putStrLn "step   - step a world once"
  putStrLn "print  - print a world"
  continue


makeExistentialRule :: Package.Rule -> RuleImpl
makeExistentialRule rule = DynRI (Package.toDynamicRule rule)

main :: IO ()
main = do
  putStrLn "Welcome to the world of cellular automata!"

  _ <- printHelp

  appState <- createAppState

  args <- getArgs

  case args of
    (ruleFile : []) -> do
      let ruleFile' = "./BookSamples/CH05/ch5/data/packages" <> ruleFile
      putStrLn $ "\nRule file: " <> ruleFile'

      ruleStr <- readFile ruleFile'
      let rule = read ruleStr
      print rule

      let existRule = makeExistentialRule rule

      addRule appState existRule

      putStrLn "Rule added."
    _ -> pure ()

  go appState

go :: AppState -> IO ()
go appState = do
  putStrLn "\nType a command:"
  cmd <- getLine

  appAction <- case filter (/=' ') cmd of
    "quit"   -> finish
    "help"   -> printHelp
    "rules"  -> EApp.processListRuleCodes appState
    "worlds" -> EApp.processListWorlds appState
    "load"   -> EApp.processLoad appState
    "predef" -> EApp.processLoadPredef appState
    "step"   -> EApp.processStep appState
    "print"  -> EApp.processPrint appState
    _ -> continueWithMsg "Unknown command. Type `help` to see the list of commands."

  case appAction of
    AppFinish (Just msg) -> putStrLn msg
    AppFinish _ -> pure ()
    AppContinue (Just msg) -> do
      putStrLn msg
      go appState
    AppContinue _ -> go appState
