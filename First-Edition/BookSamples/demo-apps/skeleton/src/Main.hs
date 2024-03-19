module Main where

import Skeleton.App.App
import Skeleton.App.State
import Skeleton.App.Action
import Skeleton.Machine.Interface
import Skeleton.Machine.Language
import Skeleton.App.Storage
import Skeleton.Assets.BinaryIncrement
import Skeleton.Assets.SimpleRule

import qualified Data.Map as Map
import Data.IORef (IORef, newIORef)
import Data.Proxy
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)


printHelp :: IO AppAction
printHelp = do
  putStrLn "\nOptional arg: path_to_external_rule"

  putStrLn "\nCommands:"
  putStrLn "help    - this help message"
  putStrLn "quit    - exit"
  putStrLn "rules   - list available rules"
  putStrLn "load    - load a rule"
  putStrLn "predef  - load predefined machines"
  putStrLn "machines - list active machines"
  putStrLn "run     - run a machine"
  putStrLn "print   - print a machine"
  continue


-- makeExistentialRule :: Package.Rule -> RuleImpl
-- makeExistentialRule rule = DynRI (Package.toDynamicRule rule)

-- createAppState :: IO AppState
-- createAppState = do
--   rulesRef  <- newIORef Map.empty
--   worldsRef <- newIORef Map.empty
--   pure $ AppState rulesRef worldsRef

main :: IO ()
main = do
  putStrLn "Welcome to the world of things!"

  _ <- printHelp

  pure ()

  -- appState <- createAppState

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
