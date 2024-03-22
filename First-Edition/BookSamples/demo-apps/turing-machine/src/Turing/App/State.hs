-- | Application mutable state.

module Turing.App.State where

import Turing.Machine.Language
import Turing.App.Storage

import qualified Data.Map as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef)


data AppState = AppState
  { asRulesRef :: IORef Rules
  , asTapesRef :: IORef Tapes
  }


addRule :: AppState -> RuleImpl -> IO RuleIndex
addRule (AppState rulesRef _) rule = do
  rules <- readIORef rulesRef
  let idx = Map.size rules
  let rules' = Map.insert idx rule rules
  writeIORef rulesRef rules'
  pure idx

addTape :: AppState -> Tape -> IO TapeIndex
addTape (AppState _ tapesRef) tape = do
  tapes <- readIORef tapesRef
  let idx = Map.size tapes
  let tapes' = Map.insert idx tape tapes
  writeIORef tapesRef tapes'
  pure idx

createAppState' :: Rules -> IO AppState
createAppState' rules = do
  rulesRef <- newIORef rules
  tapesRef <- newIORef Map.empty
  pure $ AppState rulesRef tapesRef

createAppState :: IO AppState
createAppState = do
  rulesRef <- newIORef Map.empty
  tapesRef <- newIORef Map.empty
  pure $ AppState rulesRef tapesRef
