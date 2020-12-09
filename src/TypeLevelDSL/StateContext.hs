{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}

-- TODO: should be abstract data type.
-- TODO: make it thread safe in the future.

module TypeLevelDSL.StateContext where

import qualified Data.Dynamic as Dyn
import qualified Data.Map as Map
import Data.IORef


type StateContext = IORef (Map.Map String (IORef Dyn.Dynamic))

createStateContext :: StateContext
createStateContext = newIORef

lookupValue :: String -> StateContext -> IO (Maybe Dyn.Dynamic)
lookupValue k ctx = do
  ctxMap <- readIORef ctx
  case Map.lookup k ctxMap of
    Just valRef -> Just <$> readIORef valRef
    Nothing -> pure Nothing

updateValue :: String -> Dyn.Dynamic -> StateContext -> IO ()
updateValue k val ctx = do
  ctxMap <- readIORef ctx
  case Map.lookup k ctxMap of
    Nothing -> do
      valRef <- newIORef val
      writeIORef $ Map.insert k valRef ctxMap
    Just valRef -> writeIORef valRef val

insertValue :: String -> Dyn.Dynamic -> StateContext -> IO ()
insertValue k val ctx = do
  ctxMap <- readIORef ctx
  case Map.lookup k ctxMap of
    Nothing -> do
      valRef <- newIORef val
      writeIORef $ Map.insert k valRef ctxMap
    Just _ -> error $ "Value " <> k <> " already exists."
