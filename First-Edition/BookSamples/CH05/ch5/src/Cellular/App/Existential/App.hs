{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cellular.App.Existential.App
  ( module X
  , processListRuleCodes
  , processListWorlds
  , processLoad
  , processLoadPredef
  , processPrint
  , processStep
  ) where

import Cellular.App.Existential.Rules (RuleImpl(..))
import Cellular.App.Existential.Worlds
    ( Worlds, WorldIndex, WorldInstance(..), Generation )
import Cellular.App.Existential.Worlds as X (Worlds)
import Cellular.App.State (AppState(..))
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
  :: forall rule           -- Brings `rule` into the scope of body
   . IAutomaton () rule    -- Demands the `rule` to be automaton.
  => Proxy rule            -- Highlights what rule type was requrested by the caller.
  -> FilePath
  -> IO (Either String WorldInstance)
loadWorld2 _ path = do
  eBoard <- try (loadFromFile2 path)
  case eBoard of
    Left (err :: SomeException) -> pure (Left (show err))
    Right board -> pure (Right (WI @rule 0 (CW board)))    -- specifying the automaton rule


loadWorld2Dyn
  :: FilePath
  -> DynamicRule
  -> IO (Either String WorldInstance)
loadWorld2Dyn path dynRule = do
  eBoard <- try (loadFromFile2 path)
  case eBoard of
    Left (err :: SomeException) -> pure (Left (show err))
    Right board -> pure (Right (DynWI dynRule 0 (CW board)))

-- App interface

processListRuleCodes :: AppState -> IO AppAction
processListRuleCodes (AppState rulesRef _) = do
  rules <- readIORef rulesRef
  putStrLn "\nSupported rules ([code] name):"
  mapM_ f $ Map.toList rules
  continue
  where
    f (ruleCode, RI proxy) =
      putStrLn ("[" <> ruleCode <> "] (static) " <> name () proxy)
    f (ruleCode, DynRI dynRule) =
      putStrLn ("[" <> ruleCode <> "] (dynamic) " <> name dynRule (Proxy @'DynRule))


processListWorlds :: AppState -> IO AppAction
processListWorlds (AppState _ worldsRef) = do
  worlds <- readIORef worldsRef
  putStrLn ("\nWorlds available: " <> show (Map.size worlds))
  let ws = Map.toAscList worlds
  mapM_ f ws
  continue
  where
    f :: (WorldIndex, WorldInstance) -> IO ()
    f (idx, WI gen cw) = f' idx gen cw
    f (idx, DynWI dynRule gen cw) = fDyn' dynRule idx gen cw
    f' :: forall rule
        . IAutomaton () rule
       => WorldIndex
       -> Generation
       -> CellWorld rule
       -> IO ()
    f' idx gen _ = do
      let strCode = code () (Proxy @rule)
      putStrLn (show idx <> ") [" <> strCode <> "], gen: " <> show gen)

    fDyn' dynRule idx gen _ = do
      let strCode = code dynRule (Proxy @'DynRule)
      putStrLn (show idx <> ") [" <> strCode <> "], gen: " <> show gen)

processStep :: AppState -> IO AppAction
processStep (AppState _ worldsRef) = do
  putStrLn "\nEnter world index to step:"
  idxStr <- getLine
  case readMaybe idxStr of
    Nothing -> continueWithMsg "Invalid index."
    Just idx -> do
      worlds <- readIORef worldsRef
      case Map.lookup idx worlds of
        Nothing -> continueWithMsg "Index doesn't exist."
        Just (WI gen world) -> do
          let world' = step () world
          let wi = WI (gen + 1) world'
          let worlds' = Map.insert idx wi worlds
          writeIORef worldsRef worlds'
          continue
        Just (DynWI dynRule gen world) -> do
          let world' = step dynRule world
          let wi = DynWI dynRule (gen + 1) world'
          let worlds' = Map.insert idx wi worlds
          writeIORef worldsRef worlds'
          continue

processPrint :: AppState -> IO AppAction
processPrint (AppState _ worldsRef) = do
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
        Just (DynWI _ _ (CW board)) -> do
          printBoard board
          continue

processLoad :: AppState -> IO AppAction
processLoad appState@(AppState rulesRef worldsRef) = do
  _ <- processListRuleCodes appState
  putStrLn "\nEnter rule code:"
  ruleCode <- getLine

  rules <- readIORef rulesRef

  case Map.lookup ruleCode rules of
    Nothing -> continueWithMsg "Unknown rule."
    Just (RI proxy) -> do
      putStrLn "\nEnter world path:"
      file <- getLine

      eWI <- loadWorld2 proxy file

      case eWI of
        Left err  -> continueWithMsg ("Failed to load [" <> ruleCode <> "]: " <> err)
        Right wi -> do
          worlds <- readIORef worldsRef
          let idx = Map.size worlds
          let worlds' = Map.insert idx wi worlds
          writeIORef worldsRef worlds'
          continueWithMsg ("Successfully loaded [" <> ruleCode <> "], index: " <> show idx)


    Just (DynRI dynRule) -> do
      putStrLn "\nEnter world path:"
      file <- getLine

      eWI <- loadWorld2Dyn file dynRule

      case eWI of
        Left err  -> continueWithMsg ("Failed to load [" <> ruleCode <> "]: " <> err)
        Right wi -> do
          worlds <- readIORef worldsRef
          let idx = Map.size worlds
          let worlds' = Map.insert idx wi worlds
          writeIORef worldsRef worlds'
          continueWithMsg ("Successfully loaded [" <> ruleCode <> "], index: " <> show idx)


processLoadPredef :: AppState -> IO AppAction
processLoadPredef (AppState rulesRef worldsRef) = do
  let predefs = [ ("gol", "./data/GoL/glider.txt")
                , ("seeds", "./data/Seeds/world1.txt")
                ]

  putStrLn "\nPredefined worlds to load:"
  mapM_ (\(c, f) -> putStrLn $ "[" <> c <> "]: " <> f) predefs

  execPath <- getCurrentDirectory     -- for some reason returns the stack.yaml containing folder

  rules <- readIORef rulesRef

  rs <- for predefs $ \(c, f) -> do
    case Map.lookup c rules of
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

      Just (DynRI dynRule) -> do
        let file = execPath <> "/BookSamples/CH05/ch5/" <> f
        eWI <- loadWorld2Dyn file dynRule
        case eWI of
          Left err  -> pure ("Failed to load [" <> c <> "]: " <> err)
          Right wi -> do
            worlds <- readIORef worldsRef
            let idx = Map.size worlds
            let worlds' = Map.insert idx wi worlds
            writeIORef worldsRef worlds'
            pure ("Successfully loaded [" <> c <> "], index: " <> show idx)

  continueWithMsg $ intercalate "\n" rs
