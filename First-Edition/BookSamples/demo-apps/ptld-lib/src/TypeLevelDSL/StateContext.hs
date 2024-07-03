{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}

-- TODO: should be abstract data type.
-- TODO: make it thread safe in the future.

module TypeLevelDSL.StateContext where

import Prelude

import qualified Data.Dynamic as Dyn
import qualified Data.Map as Map
import Data.IORef
import Data.Text (Text)

import TypeLevelDSL.Context (Context, Key, getDyn, setDyn, getSubContext)


-- Previous version:
-- newtype StateContext = StateContext (IORef (Map.Map Text (IORef Dyn.Dynamic)))

-- But we don't exposure the internal dynamic reference anyway,
-- and there is none consumers of this secret IORef,
-- so no need to keep a mutable value.
newtype StateContext = StateContext (IORef (Map.Map Key Dyn.Dynamic))

instance Context StateContext where
  getDyn ctx k     = lookupValue k ctx
  setDyn ctx k val = updateValue k val ctx
  getSubContext _ _ = pure Nothing

createStateContext :: IO StateContext
createStateContext = StateContext <$> newIORef Map.empty

fromList :: [(Key, Dyn.Dynamic)] -> IO StateContext
fromList l = do
  stCtx <- createStateContext
  mapM_ (\(k, v) -> insertValue k v stCtx) l
  pure stCtx

lookupValue :: Key -> StateContext -> IO (Maybe Dyn.Dynamic)
lookupValue k (StateContext mapRef) = do
  ctxMap <- readIORef mapRef
  pure $ Map.lookup k ctxMap

insertValue :: Key -> Dyn.Dynamic -> StateContext -> IO ()
insertValue k val (StateContext mapRef) = do
  ctxMap <- readIORef mapRef
  writeIORef mapRef $ Map.insert k val ctxMap

updateValue :: Key -> Dyn.Dynamic -> StateContext -> IO ()
updateValue = insertValue
