{-# LANGUAGE TypeApplications #-}
module Existential.App
  ( module X
  , processListRuleCodes
  , processListWorlds
  , processLoad
  , processPrint
  , processStep
  ) where

import Existential.Rules
    ( supportedRules, supportedRulesDict, RuleImpl(..) )
import Existential.Worlds ( WorldIndex, WorldInstance(..), Worlds )
import Existential.Worlds as X (Worlds)
import Board ( printBoard )
import Automaton ( loadFromFile, Automaton(..), CellWorld(CW) )
import App ( AppAction, continueWithMsg, continue )

import qualified Data.Map as Map
import Data.Proxy (Proxy)
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Control.Exception ( SomeException, try )
import Text.Read (readMaybe)


loadWorld
  :: forall rule        -- Brings `rule` into the scope of body
   . Automaton rule     -- Demands the `rule` to be automaton.
  => IORef Worlds
  -> RuleImpl
  -> Proxy rule         -- Highlights what rule type was requrested by the caller.
  -> FilePath
  -> IO (Either String WorldIndex)
loadWorld worldsRef ri proxy path = do
  eCellWorld <- try (loadFromFile path)
  case eCellWorld of
    Left (err :: SomeException) -> pure (Left (show err))
    Right world -> do
      worlds <- readIORef worldsRef

      let idx = Map.size worlds
      let w = WI @rule ri 0 world    -- specifying the automaton rule
      let worlds' = Map.insert idx w worlds

      writeIORef worldsRef worlds'
      pure (Right idx)


-- Simplified snippet for the with no error processing for the book
loadWorld'
  :: forall rule        -- Brings `rule` into the scope of body
   . Automaton rule     -- Demands the `rule` to be automaton.
  => IORef Worlds
  -> RuleImpl
  -> Proxy rule         -- Highlights what rule type was requrested by the caller.
  -> FilePath
  -> IO WorldIndex
loadWorld' worldsRef ri proxy path = do
  world <- loadFromFile path

  worlds <- readIORef worldsRef

  let w = WI @rule ri 0 world    -- specifying the automaton rule

  let idx = Map.size worlds
  let worlds' = Map.insert idx w worlds
  writeIORef worldsRef worlds'

  pure idx


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
    f (idx, WI (RI proxy) gen _) = do
      let strCode = code proxy
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
        Just (WI ri gen world) -> do
          let world' = step world
          let wi = WI ri (gen + 1) world'
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
        Just (WI _ _ (CW board)) -> do
          printBoard board
          continue

processLoad :: IORef Worlds -> IO AppAction
processLoad worldsRef = do
  _ <- processListRuleCodes
  putStrLn "\nEnter rule code:"
  ruleCode <- getLine

  case Map.lookup ruleCode supportedRulesDict of
    Nothing -> continueWithMsg "Unknown rule."
    Just ri@(RI proxy) -> do
      putStrLn "\nEnter world path:"
      path <- getLine

      eIndex <- loadWorld worldsRef ri proxy path

      case eIndex of
        Left err  -> continueWithMsg ("Failed to load [" <> ruleCode <> "]: " <> err)
        Right idx -> continueWithMsg ("Successfully loaded [" <> ruleCode <> "], index: " <> show idx)
