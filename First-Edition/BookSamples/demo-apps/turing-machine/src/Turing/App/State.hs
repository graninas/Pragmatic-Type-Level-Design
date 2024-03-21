-- | Application mutable state.

module Turing.App.State where

import Turing.App.Storage

import qualified Data.Map as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef)


data AppState = AppState
  { asRulesRef :: IORef Rules
  , asTapesRef :: IORef Tapes
  }


addRule :: AppState -> RuleImpl -> IO ()
addRule (AppState rulesRef _) rule = do
  rules <- readIORef rulesRef
  let rules' = Map.insert (getName rule) rule rules
  writeIORef rulesRef rules'

createAppState :: IO AppState
createAppState = do
  rulesRef <- newIORef Map.empty
  tapesRef <- newIORef Map.empty
  pure $ AppState rulesRef tapesRef
