module Cellular.App.State where

import Cellular.App.Existential.Rules
import Cellular.App.Existential.Worlds

import qualified Data.Map as Map
import Data.IORef (IORef, readIORef, writeIORef)


data AppState = AppState
  { asRulesRef  :: IORef Rules
  , asWorldsRef :: IORef Worlds
  }


addRule :: AppState -> RuleImpl -> IO ()
addRule (AppState rulesRef _) rule = do
  rules <- readIORef rulesRef
  let rules' = Map.insert (getCode rule) rule rules
  writeIORef rulesRef rules'
