{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Main where

import qualified App.Existential.App as EApp

import qualified Data.Map as Map
import Data.IORef ( IORef, newIORef )
import App.Action (AppAction (..), continue, finish, continueWithMsg)

import Data.Proxy

import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.Seeds
import Cellular.Assets.Automata.Replicator


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
