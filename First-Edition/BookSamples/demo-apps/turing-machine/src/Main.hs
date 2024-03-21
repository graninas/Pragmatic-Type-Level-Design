module Main where

import Turing.App.App
import Turing.App.State
import Turing.App.Action
import Turing.App.Commands
import Turing.App.Storage
import Turing.Machine.Interface
import Turing.Machine.Language
import Turing.Assets.BinaryIncrement
import Turing.Assets.SimpleRule

import qualified Data.Map as Map
import Data.Proxy
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)


printHelp :: IO AppAction
printHelp = do
  putStrLn "\nOptional arg: path_to_external_rule"
  printCommandsHelp
  continue


-- makeExistentialRule :: Package.Rule -> RuleImpl
-- makeExistentialRule rule = DynRI (Package.toDynamicRule rule)


main :: IO ()
main = do
  putStrLn "Welcome to the world of things!"

  _ <- printHelp

  appState <- createAppState

  pure ()
  -- args <- getArgs

  -- case args of
  --   (ruleFile : []) -> do
  --     let ruleFile' = "./BookSamples/CH05/ch5/data/packages" <> ruleFile
  --     putStrLn $ "\nRule file: " <> ruleFile'

  --     ruleStr <- readFile ruleFile'
  --     let rule = read ruleStr
  --     print rule

  --     let existRule = makeExistentialRule rule

  --     addRule appState existRule

  --     putStrLn "Rule added."
  --   _ -> pure ()

  -- go appState

-- go :: AppState -> IO ()
-- go appState = do
--   putStrLn "\nType a command:"
--   cmd <- getLine

--   appAction <- case filter (/=' ') cmd of
--     "quit"   -> finish
--     "help"   -> printHelp
--     "rules"  -> EApp.processListRuleCodes appState
--     "worlds" -> EApp.processListWorlds appState
--     "load"   -> EApp.processLoad appState
--     "predef" -> EApp.processLoadPredef appState
--     "step"   -> EApp.processStep appState
--     "print"  -> EApp.processPrint appState
--     _ -> continueWithMsg "Unknown command. Type `help` to see the list of commands."

--   case appAction of
--     AppFinish (Just msg) -> putStrLn msg
--     AppFinish _ -> pure ()
--     AppContinue (Just msg) -> do
--       putStrLn msg
--       go appState
--     AppContinue _ -> go appState
