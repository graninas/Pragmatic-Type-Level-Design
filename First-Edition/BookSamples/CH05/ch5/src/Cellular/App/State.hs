module Cellular.App.State where

import Cellular.App.Storage.Rules
import Cellular.App.Storage.Worlds

import qualified Data.Map as Map
import Data.IORef (IORef, readIORef, writeIORef)


-- | Application state.

data AppState = AppState
  { asRulesRef  :: IORef Rules
  , asWorldsRef :: IORef Worlds
  }


addRule :: AppState -> RuleImpl -> IO ()
addRule (AppState rulesRef _) rule = do
  rules <- readIORef rulesRef
  let rules' = Map.insert (getCode rule) rule rules
  writeIORef rulesRef rules'
