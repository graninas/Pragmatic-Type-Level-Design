module Valuefied.App
  ( module X
  , processListRuleCodes
  , processListWorlds
  , processLoad
  , processPrint
  , processStep
  ) where

import Board ( printBoard )
import Valuefied.Rules
    ( supportedRulesDict, supportedRules, RuleImpl(RuleImpl) )
import Valuefied.Worlds
    ( Worlds, WorldIndex, WorldInstance(WorldInstance) )
import Valuefied.Worlds as X (Worlds)
import App ( AppAction, continueWithMsg, continue )

import qualified Data.Map as Map
import Data.Proxy (Proxy)
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Control.Exception ( SomeException, try )
import Text.Read (readMaybe)


loadWorld
  :: IORef Worlds
  -> RuleImpl
  -> FilePath
  -> IO (Either String WorldIndex)
loadWorld worldsRef ruleImpl@(RuleImpl _ _ loadF _) path = do
  eBoard <- try (loadF path)
  case eBoard of
    Left (err :: SomeException) -> pure (Left (show err))
    Right board -> do
      worlds <- readIORef worldsRef

      let idx = Map.size worlds
      let w = WorldInstance ruleImpl 0 board
      let worlds' = Map.insert idx w worlds

      writeIORef worldsRef worlds'
      pure (Right idx)


-- App interface

processListRuleCodes :: IO AppAction
processListRuleCodes = do
  putStrLn "\nSupported rules ([code] name):"
  mapM_ f supportedRules
  continue
  where
    f (ruleCode, RuleImpl ruleName _ _ _) = putStrLn ("[" <> ruleCode <> "] " <> ruleName)

processListWorlds :: IORef Worlds -> IO AppAction
processListWorlds worldsRef = do
  worlds <- readIORef worldsRef
  putStrLn ("\nWorlds available: " <> show (Map.size worlds))
  let ws = Map.toAscList worlds
  mapM_ f ws
  continue
  where
    f :: (WorldIndex, WorldInstance) -> IO ()
    f (idx, WorldInstance (RuleImpl _ ruleCode _ _) gen _) = do
      putStrLn (show idx <> ") [" <> ruleCode <> "], gen: " <> show gen)

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
        Just (WorldInstance ri@(RuleImpl _ _ _ step') gen board) -> do
          let board' = step' board
          let wi = WorldInstance ri (gen + 1) board'
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
        Just (WorldInstance _ _ board) -> do
          printBoard board
          continue

processLoad :: IORef Worlds -> IO AppAction
processLoad worldsRef = do
  _ <- processListRuleCodes
  putStrLn "\nEnter rule code:"
  ruleCode <- getLine

  case Map.lookup ruleCode supportedRulesDict of
    Nothing -> continueWithMsg "Unknown world type."
    Just ruleImpl -> do
      putStrLn "\nEnter world path:"
      path <- getLine

      eIndex <- loadWorld worldsRef ruleImpl path

      case eIndex of
        Left err  -> continueWithMsg ("Failed to load [" <> ruleCode <> "]: " <> err)
        Right idx -> continueWithMsg ("Successfully loaded [" <> ruleCode <> "], index: " <> show idx)