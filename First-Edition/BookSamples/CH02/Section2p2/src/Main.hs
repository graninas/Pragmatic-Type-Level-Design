module Main where

import Board (Board)
import Cell (Cell(..))
import Automaton ( iterateWorld, loadFromFile, saveToFile, name, Automaton, CellWorld (..), RuleCode )
import Rules

import qualified Data.Map as Map
import Data.Proxy (Proxy)
import Control.Monad (unless)


-- iterateWorlds :: Int -> Worlds -> Worlds
-- iterateWorlds n worlds | n == 0 = worlds
-- iterateWorlds n _ | n < 0 = error "Invalid iteration count"
-- iterateWorlds n [] = []
-- iterateWorlds n (MkWorld w:ws) = let
--   w' = iterateWorld 5 w
--   mkW' = MkWorld w'
--   in mkW' : iterateWorlds n ws


listRuleCodes :: IO ()
listRuleCodes = mapM_ f supportedRules
  where
    f (code, RuleImpl proxy) = putStrLn ("[" <> code <> "] " <> name proxy)


loadWorld :: RuleCode -> FilePath -> IO Bool
loadWorld _ _ = error "Not implemented"


processLoad :: IO ()
processLoad = do
  putStrLn "\nSupported rules to load ([code] name):"
  listRuleCodes

  putStrLn "\nEnter rule code:"
  ruleCode <- getLine
  case Map.lookup ruleCode supportedRulesDict of
    Nothing -> putStrLn "Unknown world type." >> go
    Just (RuleImpl proxy) -> do
      putStrLn "\nEnter world path:"
      path <- getLine
      success <- loadWorld ruleCode path
      unless success $ putStrLn ("\nFailed to load " <> ruleCode)
      go


main :: IO ()
main = do
  putStrLn "Welcome to the world of cellular automata!"
  go

go :: IO ()
go = do
  putStrLn "Type a command:"
  cmd <- getLine

  case cmd of
    "load" -> processLoad
    "q" -> pure ()
    "quit" -> pure ()
    _ -> do
      putStrLn "Unknown command."
      go

  -- gol1   :: GoL <- loadFromFile "./data/GoL/glider.txt"
  -- seeds1 :: Seeds <- loadFromFile "./data/Seeds/world1.txt"

  -- let worlds1 = [MkWorld gol1, MkWorld seeds1]
  -- let worlds2 = iterateWorlds 5 worlds1

  -- case worlds2 of
  --   [MkWorld gol2, MkWorld seeds2] -> do
  --     print (name gol2)
  --     print (name seeds1)
