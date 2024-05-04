module Main where

import Turing.App.Application
import Turing.App.State
import Turing.App.Action
import Turing.App.Commands
import Turing.App.Storage
import Turing.App.InterfacingSwitch
import Turing.Machine.Interface.TypeClass
import Turing.Machine.Language

import qualified Data.Map as Map
import Data.Proxy
import Text.Read (readMaybe)
import System.Environment (getArgs)


printHelp :: IO AppAction
printHelp = do
  putStrLn "Command-line arguments: "
  putStrLn "type-class  -  type-class interface mechanism (default)"
  putStrLn "free-monad  -  Free monad interface mechanism"
  printCommandsHelp
  continue

main :: IO ()
main = do
  putStrLn "Welcome to the world of things!"

  _ <- printHelp

  args <- getArgs
  iSwitch <- case args of
    ("free-monad" : []) -> do
      putStrLn "\nFree monad interface will be used."
      pure FreeMonad
    [] -> do
      putStrLn "\nType class interface will be used."
      pure TypeClass
    ("type-class" : []) -> do
      putStrLn "Type class interface will be used."
      pure TypeClass
    _ -> do
      putStrLn "\nUnknown argument. Type class interface will be used."
      pure TypeClass

  appState <- createAppState

  go iSwitch appState

go :: InterfacingSwitch -> AppState -> IO ()
go iSwitch appState = do

  putStrLn "\nType a command:"
  line <- getLine

  appAction <- case readMaybe line of
    Just cmd -> runCommand iSwitch cmd appState
    _ -> continueWithMsg "Unknown command. Type `Help` to see the list of commands."

  case appAction of
    AppFinish (Just msg) -> putStrLn msg
    AppFinish _ -> pure ()
    AppContinue (Just msg) -> do
      putStrLn msg
      go iSwitch appState
    AppContinue _ -> go iSwitch appState
