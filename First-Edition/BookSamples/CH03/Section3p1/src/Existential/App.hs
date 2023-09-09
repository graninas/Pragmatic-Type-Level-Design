{-# LANGUAGE TypeApplications #-}
module Existential.App
  ( module X
  , AppState (..)
  , processListRuleCodes
  , processListWorlds
  , processLoad
  , processPrint
  , processStep
  , processAddRule
  ) where

import Existential.Rules
import Existential.Worlds
import Existential.Worlds as X (Worlds)
import Board
import Automaton
import App ( AppAction, continueWithMsg, continue )

import qualified Data.Map as Map
import Data.Proxy
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Control.Exception ( SomeException, try )
import Text.Read (readMaybe)


data AppState = AppState
  { rulesRef  :: IORef Rules
  , worldsRef :: IORef Worlds
  }


loadWorld
  :: forall rule        -- Brings `rule` into the scope of body
   . Automaton rule     -- Demands the `rule` to be automaton.
  => Proxy rule         -- Highlights what rule type was requrested by the caller.
  -> FilePath
  -> IO (Either String WorldInstance)
loadWorld proxy path = do
  eCellWorld <- try (loadFromFile path)
  case eCellWorld of
    Left (err :: SomeException) -> pure (Left (show err))
    Right world -> pure (Right (WI @rule 0 world))    -- specifying the automaton rule


-- App interface

processListRuleCodes :: AppState -> IO AppAction
processListRuleCodes (AppState rulesRef _) = do
  rules <- readIORef rulesRef
  putStrLn "\nSupported rules ([code] name):"
  mapM_ f (Map.toList rules)
  continue
  where
    f (ruleCode, RI proxy) = putStrLn ("[" <> ruleCode <> "] " <> name proxy)


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
    f' :: forall rule
        . Automaton rule
       => WorldIndex
       -> Generation
       -> CellWorld rule
       -> IO ()
    f' idx gen _ = do
      let strCode = code (Proxy @rule)
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
          let world' = step world
          let wi = WI (gen + 1) world'
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

processLoad :: AppState -> IO AppAction
processLoad appSt@(AppState rulesRef worldsRef) = do
  _ <- processListRuleCodes appSt

  putStrLn "\nEnter rule code:"
  ruleCode <- getLine

  rules <- readIORef rulesRef

  case Map.lookup ruleCode rules of
    Nothing -> continueWithMsg "Unknown rule."
    Just (RI proxy) -> do
      putStrLn "\nEnter world path:"
      path <- getLine

      eWI <- loadWorld proxy path

      case eWI of
        Left err  -> continueWithMsg ("Failed to load [" <> ruleCode <> "]: " <> err)
        Right wi -> do
          worlds <- readIORef worldsRef
          let idx = Map.size worlds
          let worlds' = Map.insert idx wi worlds
          writeIORef worldsRef worlds'
          continueWithMsg ("Successfully loaded [" <> ruleCode <> "], index: " <> show idx)



processAddRule :: AppState -> IO AppAction
processAddRule _ = error "not impl"
