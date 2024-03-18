module Main where

import Skeleton.App.App
import Skeleton.App.State
import Skeleton.App.Action
import Skeleton.Assets.Script
import Skeleton.Interface
import Skeleton.Language.Domain
import Skeleton.App.Storage
import qualified Skeleton.App.Package.Thing as Package

import qualified Data.Map as Map
import Data.IORef (IORef, newIORef)
import Data.Proxy
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)


printHelp :: IO AppAction
printHelp = do
  putStrLn "\nOptional arg: path_to_external_thing"

  putStrLn "\nCommands:"
  putStrLn "help    - this help message"
  putStrLn "quit    - exit"
  putStrLn "ts      - list transforms"
  putStrLn "load    - load st"
  putStrLn "predef  - load predefined objects"
  putStrLn "objects - list active objects"
  putStrLn "act     - make an object to act"
  putStrLn "print   - print an object"
  continue


makeExistentialRule :: Package.Rule -> RuleImpl
makeExistentialRule rule = DynRI (Package.toDynamicRule rule)

createAppState :: IO AppState
createAppState = do
  rulesRef  <- newIORef Map.empty
  worldsRef <- newIORef Map.empty
  pure $ AppState rulesRef worldsRef

main :: IO ()
main = do
  putStrLn "Welcome to the world of things!"

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
