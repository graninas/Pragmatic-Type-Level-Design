module Main where

import Turing.App.App
import Turing.App.State
import Turing.App.Action
import Turing.App.Commands
import Turing.App.Storage
import Turing.Machine.Interface
import Turing.Machine.Language

import qualified Data.Map as Map
import Data.Proxy
import Text.Read (readMaybe)


printHelp :: IO AppAction
printHelp = do
  printCommandsHelp
  continue

main :: IO ()
main = do
  putStrLn "Welcome to the world of things!"

  _ <- printHelp

  appState <- createAppState

  go appState

go :: AppState -> IO ()
go appState = do
  putStrLn "\nType a command:"
  line <- getLine

  appAction <- case readMaybe line of
    Just cmd -> runCommand cmd appState
    _ -> continueWithMsg "Unknown command. Type `Help` to see the list of commands."

  case appAction of
    AppFinish (Just msg) -> putStrLn msg
    AppFinish _ -> pure ()
    AppContinue (Just msg) -> do
      putStrLn msg
      go appState
    AppContinue _ -> go appState
