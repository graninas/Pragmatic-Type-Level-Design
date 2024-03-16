{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE DataKinds #-}

module Main where

import qualified Cellular.App.Existential.App as EApp
import Cellular.App.Action (AppAction (..), continue, finish, continueWithMsg)
import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.Seeds
import Cellular.Assets.Automata.Replicator

import qualified Data.Map as Map
import Data.IORef (IORef, newIORef)
import Data.Proxy
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)


import Cellular.Automaton
import Cellular.Language.Automaton
import Cellular.App.Config
import Cellular.App.Existential.Rules


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


makeRule :: Config -> (RuleCode, RuleImpl)
makeRule (Cfg (CfgRule c d t) r) = (c, existRule)
  where
    existRule = error "TODO"


main :: IO ()
main = do
  putStrLn "Welcome to the world of cellular automata!"

  -- _ <- printHelp

  args <- getArgs

  worldsRef <- case args of
    (cfgFile : []) -> do
      let cfgFile' = "./BookSamples/CH05/ch5/" <> cfgFile
      putStrLn $ "Cfg file: " <> cfgFile'

      cfgStr <- readFile cfgFile'
      let (cfg :: Config) = read cfgStr
      print cfg

      let existRule = makeRule cfg

      worldsRef <- newIORef Map.empty

      pure worldsRef
    _ -> newIORef Map.empty

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
