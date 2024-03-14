{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Main where

import qualified Cellular.App.Existential.App as EApp
import Cellular.App.Action (AppAction (..), continue, finish, continueWithMsg)
import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.Seeds
import Cellular.Assets.Automata.Replicator

import qualified Data.Map as Map
import Data.IORef (IORef, newIORef)
import Data.Proxy


printHelp :: IO AppAction
printHelp = do
  putStrLn "\nCommands:"
  putStrLn "help   - this help message"
  putStrLn "quit   - exit"
  putStrLn "rules  - list supported rules"
  putStrLn "load   - load a world"
  putStrLn "predef - load predefined worlds"
  putStrLn "worlds - list loaded worlds"
  putStrLn "step   - step a world once"
  putStrLn "print  - print a world"
  continue


main :: IO ()
main = do
  putStrLn "Welcome to the world of cellular automata!"
  putStrLn "\nN.B., only 2-dimensional automata supported now."
  _ <- printHelp

  worldsRef <- newIORef Map.empty
  go worldsRef

go :: IORef EApp.Worlds -> IO ()
go worldsRef = do
  putStrLn "\nType a command:"
  cmd <- getLine

  appAction <- case filter (/=' ') cmd of
    "quit"   -> finish
    "help"   -> printHelp
    "rules"  -> EApp.processListRuleCodes
    "worlds" -> EApp.processListWorlds worldsRef
    "load"   -> EApp.processLoad worldsRef
    "predef" -> EApp.processLoadPredef worldsRef
    "step"   -> EApp.processStep worldsRef
    "print"  -> EApp.processPrint worldsRef
    _ -> continueWithMsg "Unknown command. Type `help` to see the list of commands."

  case appAction of
    AppFinish (Just msg) -> putStrLn msg
    AppFinish _ -> pure ()
    AppContinue (Just msg) -> do
      putStrLn msg
      go worldsRef
    AppContinue _ -> go worldsRef
