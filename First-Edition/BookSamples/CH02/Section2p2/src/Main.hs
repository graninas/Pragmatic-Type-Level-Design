module Main where

import qualified Valuefied.App as VApp
import qualified Existential.App as EApp

import qualified Data.Map as Map
import Data.IORef ( IORef, newIORef )
import App (AppAction (..), continue, finish, continueWithMsg)

-- Won't compile:

-- worlds1 :: Map.Map String (CellWorld rule)
-- worlds1 = Map.fromList
--   [ ("Game of Life", golWorld)
--   , ("Seeds", seedsWorld)
--   , ("Replicator", replicatorWorld)
--   ]

-- worlds2 :: Automaton rule => Map.Map String (CellWorld rule)
-- worlds2 = Map.fromList [("Game of Life", golWorld), ("Seeds", seedsWorld)]

-- worlds3 :: Automaton rule => Map.Map String (CellWorld rule)
-- worlds3 =
--   Map.insert "Game of Life" golWorld
--   (Map.insert "Seeds" seedsWorld Map.empty)


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

go :: IORef VApp.Worlds -> IO ()
go worldsRef = do
  putStrLn "\nType a command:"
  cmd <- getLine

  appAction <- case filter (/=' ') cmd of
    "quit"   -> finish
    "help"   -> printHelp
    "rules"  -> VApp.processListRuleCodes
    "worlds" -> VApp.processListWorlds worldsRef
    "load"   -> VApp.processLoad worldsRef
    "step"   -> VApp.processStep worldsRef
    "print"  -> VApp.processPrint worldsRef
    _ -> continueWithMsg "Unknown command. Type `help` to see the list of commands."

  case appAction of
    AppFinish (Just msg) -> putStrLn msg
    AppFinish _ -> pure ()
    AppContinue (Just msg) -> do
      putStrLn msg
      go worldsRef
    AppContinue _ -> go worldsRef
