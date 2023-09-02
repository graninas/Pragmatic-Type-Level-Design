{-# LANGUAGE TypeApplications #-}
module Main where

import Board (loadBoardFromFile, printBoard)
import Automaton ( Automaton(..), CellWorld(CW) )
import Rules
    ( supportedRulesDict, supportedRules, RuleImpl(RuleImpl) )
import Worlds ( Worlds, WorldInstance(..), WorldIndex )

import qualified Data.Map as Map
import Data.Proxy (Proxy)
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Control.Exception ( SomeException, try )
import Text.Read (readMaybe)


-- iterateWorlds :: Int -> Worlds -> Worlds
-- iterateWorlds n worlds | n == 0 = worlds
-- iterateWorlds n _ | n < 0 = error "Invalid iteration count"
-- iterateWorlds n [] = []
-- iterateWorlds n (MkWorld w:ws) = let
--   w' = iterateWorld 5 w
--   mkW' = MkWorld w'
--   in mkW' : iterateWorlds n ws


listRuleCodes :: IO ()
listRuleCodes = do
  putStrLn "\nSupported rules ([code] name):"
  mapM_ f supportedRules
  where
    f (ruleCode, RuleImpl proxy) = putStrLn ("[" <> ruleCode <> "] " <> name proxy)

listWorlds :: IORef Worlds -> IO ()
listWorlds worldsRef = do
  worlds <- readIORef worldsRef
  putStrLn ("\nWorlds available: " <> show (Map.size worlds))
  let ws = Map.toAscList worlds
  mapM_ f ws
  where
    f :: (WorldIndex, WorldInstance) -> IO ()
    f (idx, WorldInstance proxy gen _) = do
      let strCode = code proxy
      putStrLn (show idx <> ") [" <> strCode <> "], gen: " <> show gen)

loadWorld
  :: forall rule        -- Brings `rule` into the scope of body
   . Automaton rule     -- Demands the `rule` to be automaton.
  => IORef Worlds
  -> Proxy rule         -- Highlights what rule type was requrested by the caller.
  -> FilePath
  -> IO (Either String WorldIndex)
loadWorld worldsRef proxy path = do
  eBoard <- try (loadBoardFromFile path)
  case eBoard of
    Left (err :: SomeException) -> pure (Left (show err))
    Right board -> do
      worlds <- readIORef worldsRef

      let idx = Map.size worlds
      let w = WorldInstance @rule proxy 0 (CW board)    -- specifying the automaton rule
      let worlds' = Map.insert idx w worlds

      writeIORef worldsRef worlds'
      pure (Right idx)


processStep :: IORef Worlds -> IO ()
processStep worldsRef = do
  putStrLn "\nEnter world index to step:"
  idxStr <- getLine
  case readMaybe idxStr of
    Nothing -> putStrLn "Invalid index."
    Just idx -> do
      worlds <- readIORef worldsRef
      case Map.lookup idx worlds of
        Nothing -> putStrLn "Index doesn't exist."
        Just (WorldInstance proxy gen world) -> do
          let world' = step world
          let wi = WorldInstance proxy (gen + 1) world'
          let worlds' = Map.insert idx wi worlds
          writeIORef worldsRef worlds'

processPrint :: IORef Worlds -> IO ()
processPrint worldsRef = do
  putStrLn "\nEnter world index to print:"
  idxStr <- getLine
  case readMaybe idxStr of
    Nothing -> putStrLn "Invalid index."
    Just idx -> do
      worlds <- readIORef worldsRef
      case Map.lookup idx worlds of
        Nothing -> putStrLn "Index doesn't exist."
        Just (WorldInstance proxy gen (CW board)) ->
          printBoard board


processLoad :: IORef Worlds -> IO ()
processLoad worldsRef = do
  listRuleCodes
  putStrLn "\nEnter rule code:"
  ruleCode <- getLine

  case Map.lookup ruleCode supportedRulesDict of
    Nothing -> putStrLn "Unknown world type." >> go worldsRef
    Just (RuleImpl proxy) -> do
      putStrLn "\nEnter world path:"
      path <- getLine

      eIndex <- loadWorld worldsRef proxy path

      case eIndex of
        Left err  -> putStrLn ("Failed to load [" <> ruleCode <> "]: " <> err)
        Right idx -> putStrLn ("Successfully loaded [" <> ruleCode <> "], index: " <> show idx)

printHelp :: IO ()
printHelp = do
  putStrLn "\nCommands:"
  putStrLn "help   - this help message"
  putStrLn "quit   - exit"
  putStrLn "rules  - list supported rules"
  putStrLn "load   - load a world"
  putStrLn "worlds - list loaded worlds"
  putStrLn "step   - step a world once"
  putStrLn "print  - print a world"



main :: IO ()
main = do
  putStrLn "Welcome to the world of cellular automata!"
  printHelp

  worldsRef <- newIORef Map.empty
  go worldsRef

go :: IORef Worlds -> IO ()
go worldsRef = do
  putStrLn "\nType a command:"
  cmd <- getLine

  case filter (/=' ') cmd of
    "quit"   -> pure ()
    "help"   -> printHelp >> go worldsRef
    "rules"  -> listRuleCodes >> go worldsRef
    "worlds" -> listWorlds worldsRef >> go worldsRef
    "load"   -> processLoad worldsRef >> go worldsRef
    "step"   -> processStep worldsRef >> go worldsRef
    "print"  -> processPrint worldsRef >> go worldsRef
    _ -> do
      putStrLn "Unknown command. Type `help` to see the list of commands."
      go worldsRef
