{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
module Cellular.App.Existential.App
  ( module X
  , processListRuleCodes
  , processListWorlds
  , processLoad
  , processLoadPredef
  , processPrint
  , processStep
  ) where

import Cellular.App.Existential.Rules
    ( RuleImpl(RI), supportedRules, supportedRulesDict )
import Cellular.App.Existential.Worlds
    ( Worlds, WorldIndex, WorldInstance(..), Generation )
import Cellular.App.Existential.Worlds as X (Worlds)
import Cellular.App.Action ( AppAction, continue, continueWithMsg )
import Cellular.Automaton
import Cellular.Language.Board
import Cellular.Language.Automaton

import qualified Data.Map as Map
import Data.Proxy ( Proxy(..) )
import Data.IORef ( IORef, readIORef, writeIORef )
import Data.Traversable (for)
import Data.List (intercalate)
import Control.Exception ( SomeException, try )
import Text.Read (readMaybe)
import System.Directory


printBoard :: Board -> IO ()
printBoard board = do
    let ks :: [[Int]] = Map.keys board
        xs :: [Int] = map (!! 0) ks
        ys :: [Int] = map (!! 1) ks
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        printRow b coords@[_, x]
            | x == maxX = putStrLn $ cellChar $ Map.findWithDefault 0 coords b
            | otherwise = putStr   $ cellChar $ Map.findWithDefault 0 coords b
        printRow _ _ = error "Only 2 dimension automata supported yet."
        cellChar 0 = "."
        cellChar 1 = "#"
        cellChar n = error $ "Char not supported: " <> show n
    mapM_ (printRow board) [[y, x] | y <- [minY..maxY], x <- [minX..maxX]]


loadFromFile2 :: FilePath -> IO Board
loadFromFile2 path = do
  (content :: String) <- readFile path

  let (rows :: [String]) = lines content
  let (cells :: [[StateIdx]]) = map (map toCell) rows

  pure (toBoard2 cells)
  where
    toCell :: Char -> StateIdx
    toCell 'x' = 1
    toCell _ = 0

toBoard2 :: [[StateIdx]] -> Board
toBoard2 cells
  = Map.fromList
  $ map (\((x,y), cell) -> ([x, y], cell))
  $ toBoard2' cells

toBoard2' :: [[StateIdx]] -> [((Int, Int), StateIdx)]
toBoard2' cells = let
  ixedCells :: [(Int, [(Int, StateIdx)])] = zip [1..] (map (zip [1..]) cells)
  in foldr joinCells [] ixedCells
  where
    joinCells
      :: (Int, [(Int, StateIdx)])
      -> [((Int, Int), StateIdx)]
      -> [((Int, Int), StateIdx)]
    joinCells (i, rs) lst =
      lst ++ map (\(j, cell) -> ((i, j), cell)) rs


loadWorld2
  :: forall rule        -- Brings `rule` into the scope of body
   . IAutomaton rule    -- Demands the `rule` to be automaton.
  => Proxy rule         -- Highlights what rule type was requrested by the caller.
  -> FilePath
  -> IO (Either String WorldInstance)
loadWorld2 _ path = do
  eBoard <- try (loadFromFile2 path)
  case eBoard of
    Left (err :: SomeException) -> pure (Left (show err))
    Right board -> pure (Right (WI @rule 0 (CW board)))    -- specifying the automaton rule


-- App interface

processListRuleCodes :: IO AppAction
processListRuleCodes = do
  putStrLn "\nSupported rules ([code] name):"
  mapM_ f supportedRules
  continue
  where
    f (ruleCode, RI proxy) = putStrLn ("[" <> ruleCode <> "] " <> name proxy)


processListWorlds :: IORef Worlds -> IO AppAction
processListWorlds worldsRef = do
  worlds <- readIORef worldsRef
  putStrLn ("\nWorlds available: " <> show (Map.size worlds))
  let ws = Map.toAscList worlds
  mapM_ f ws
  continue
  where
    f :: (WorldIndex, WorldInstance) -> IO ()
    f (idx, WI gen cw) = f' idx gen cw
    f' :: forall rule
        . IAutomaton rule
       => WorldIndex
       -> Generation
       -> CellWorld rule
       -> IO ()
    f' idx gen _ = do
      let strCode = code (Proxy @rule)
      putStrLn (show idx <> ") [" <> strCode <> "], gen: " <> show gen)

processStep :: IORef Worlds -> IO AppAction
processStep worldsRef = do
  putStrLn "\nEnter world index to step:"
  idxStr <- getLine
  case readMaybe idxStr of
    Nothing -> continueWithMsg "Invalid index."
    Just idx -> do
      worlds <- readIORef worldsRef
      case Map.lookup idx worlds of
        Nothing -> continueWithMsg "Index doesn't exist."
        Just (WI gen world) -> do
          let world' = step world
          let wi = WI (gen + 1) world'
          let worlds' = Map.insert idx wi worlds
          writeIORef worldsRef worlds'
          continue

processPrint :: IORef Worlds -> IO AppAction
processPrint worldsRef = do
  putStrLn "\nEnter world index to print:"
  idxStr <- getLine
  case readMaybe idxStr of
    Nothing -> continueWithMsg "Invalid index."
    Just idx -> do
      worlds <- readIORef worldsRef
      case Map.lookup idx worlds of
        Nothing -> continueWithMsg "Index doesn't exist."
        Just (WI _ (CW board)) -> do
          printBoard board
          continue

processLoad :: IORef Worlds -> IO AppAction
processLoad worldsRef = do
  _ <- processListRuleCodes
  putStrLn "\nEnter rule code:"
  ruleCode <- getLine

  case Map.lookup ruleCode supportedRulesDict of
    Nothing -> continueWithMsg "Unknown rule."
    Just (RI proxy) -> do
      putStrLn "\nEnter world path:"
      path <- getLine

      eWI <- loadWorld2 proxy path

      case eWI of
        Left err  -> continueWithMsg ("Failed to load [" <> ruleCode <> "]: " <> err)
        Right wi -> do
          worlds <- readIORef worldsRef
          let idx = Map.size worlds
          let worlds' = Map.insert idx wi worlds
          writeIORef worldsRef worlds'
          continueWithMsg ("Successfully loaded [" <> ruleCode <> "], index: " <> show idx)


processLoadPredef :: IORef Worlds -> IO AppAction
processLoadPredef worldsRef = do
  let predefs = [ ("gol", "./data/GoL/glider.txt")
                , ("seeds", "./data/Seeds/world1.txt")
                ]

  putStrLn "\nPredefined worlds to load:"
  mapM_ (\(c, f) -> putStrLn $ "[" <> c <> "]: " <> f) predefs

  execPath <- getCurrentDirectory     -- for some reason returns the stack.yaml containing folder

  rs <- for predefs $ \(c, f) -> do
    case Map.lookup c supportedRulesDict of
      Nothing -> pure "Unknown rule."
      Just (RI proxy) -> do
        let file = execPath <> "/BookSamples/CH05/ch5/" <> f
        eWI <- loadWorld2 proxy file
        case eWI of
          Left err  -> pure ("Failed to load [" <> c <> "]: " <> err)
          Right wi -> do
            worlds <- readIORef worldsRef
            let idx = Map.size worlds
            let worlds' = Map.insert idx wi worlds
            writeIORef worldsRef worlds'
            pure ("Successfully loaded [" <> c <> "], index: " <> show idx)

  continueWithMsg $ intercalate "\n" rs
