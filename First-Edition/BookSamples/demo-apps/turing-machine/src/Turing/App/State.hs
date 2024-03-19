module Turing.App.State where

import Turing.App.Storage

import qualified Data.Map as Map
import Data.IORef (IORef, readIORef, writeIORef)


data AppState = AppState
  -- { asObjectsRef :: IORef Rules
  -- , asWorldsRef :: IORef Worlds
  -- }


-- addRule :: AppState -> RuleImpl -> IO ()
-- addRule (AppState rulesRef _) rule = do
--   rules <- readIORef rulesRef
--   let rules' = Map.insert (getCode rule) rule rules
--   writeIORef rulesRef rules'
