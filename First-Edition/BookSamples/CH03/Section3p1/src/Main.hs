{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Main where

import Existential.Rules
import Existential.Worlds
import Existential.App

import qualified Data.Map as Map
import Data.IORef ( IORef, newIORef )
import App (AppAction (..), continue, finish, continueWithMsg)


printHelp :: IO AppAction
printHelp = do
  putStrLn "\nCommands:"
  putStrLn "help   - this help message"
  putStrLn "quit   - exit"
  putStrLn "rules  - list supported rules"
  putStrLn "load   - load a world"
  putStrLn "worlds - list loaded worlds"
  putStrLn "step   - step a world once"
  putStrLn "print  - print a world"
  continue

main :: IO ()
main = do
  putStrLn "Welcome to the world of cellular automata!"
  _ <- printHelp

  rulesRef  <- newIORef supportedRulesDict
  worldsRef <- newIORef Map.empty
  go (AppState rulesRef worldsRef)

go :: AppState -> IO ()
go appSt = do
  putStrLn "\nType a command:"
  cmd <- getLine

  appAction <- case filter (/=' ') cmd of
    "quit"   -> finish
    "help"   -> printHelp
    "rules"  -> processListRuleCodes appSt
    "worlds" -> processListWorlds appSt
    "load"   -> processLoad appSt
    "step"   -> processStep appSt
    "print"  -> processPrint appSt

    "add_rule"    -> processAddRule appSt
    _ -> continueWithMsg "Unknown command. Type `help` to see the list of commands."

  case appAction of
    AppFinish (Just msg) -> putStrLn msg
    AppFinish _ -> pure ()
    AppContinue (Just msg) -> do
      putStrLn msg
      go appSt
    AppContinue _ -> go appSt
