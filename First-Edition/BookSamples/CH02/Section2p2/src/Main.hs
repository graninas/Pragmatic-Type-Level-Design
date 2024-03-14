{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Main where

import qualified Cellular.App.Valuefied.App as VApp
import qualified Cellular.App.Existential.App as EApp
import Cellular.App.Action (AppAction (..), continue, finish, continueWithMsg)
import Cellular.Domain.Automaton
import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.Seeds
import Cellular.Assets.Automata.Replicator

import qualified Data.Map as Map
import Data.IORef ( IORef, newIORef )
import Data.Proxy


-- Won't compile:

-- rules1 :: [(RuleCode, Proxy rule)]
-- rules1 =
--   [ ("gol", Proxy :: Proxy GoL)
--   , ("seeds", Proxy :: Proxy Seeds)
--   , ("repl", Proxy :: Proxy Replicator)
--   ]

-- rules2 :: Automaton rule => [(RuleCode, Proxy rule)]
-- rules2 =
--   [ ("gol", Proxy :: Proxy GoL)
--   , ("seeds", Proxy :: Proxy Seeds)
--   , ("repl", Proxy :: Proxy Replicator)
--   ]

-- rules3 :: forall rule . Automaton rule => [(RuleCode, Proxy rule)]
-- rules3 =
--   ("gol", Proxy :: Proxy GoL)
--   : ("seeds", Proxy :: Proxy Seeds)
--   : ("repl", Proxy :: Proxy Replicator)
--   : []

-- worlds2 :: Automaton rule => Map.Map String (CellWorld rule)
-- worlds2 = Map.fromList [("gol", golWorld), ("Seeds", seedsWorld)]

-- worlds3 :: Automaton rule => Map.Map String (CellWorld rule)
-- worlds3 =
--   Map.insert "gol" golWorld
--   (Map.insert "seeds" seedsWorld Map.empty)



-- Won't compile:

-- worlds1 :: Map.Map String (CellWorld rule)
-- worlds1 = Map.fromList
--   [ ("gol", golWorld)
--   , ("seeds", seedsWorld)
--   , ("repl", replicatorWorld)
--   ]

-- worlds2 :: Automaton rule => Map.Map String (CellWorld rule)
-- worlds2 = Map.fromList [("gol", golWorld), ("seeds", seedsWorld)]

-- worlds3 :: Automaton rule => Map.Map String (CellWorld rule)
-- worlds3 =
--   Map.insert "gol" golWorld
--   (Map.insert "seeds" seedsWorld Map.empty)


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
