{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

module TypeLevel.ZeplrogOOP.Dynamic.Instantiation.Script
  ( makeScript
  ) where

import CPrelude

import TypeLevel.ZeplrogOOP.Static.Model
import qualified TypeLevel.ZeplrogOOP.Dynamic.Model as DMod

import TypeSelector.Granular
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map
import qualified GHC.Types as GHC
import Unsafe.Coerce (unsafeCoerce)


data IScrRuntime = IScrRuntime
  { iScrVars :: IORef (Map.Map String (IORef GHC.Any))
  }

type ScriptInterpreter a = ReaderT IScrRuntime IO a

class IScr it a | it -> a where
  iScr :: DMod.Property -> it -> ScriptInterpreter a

instance IScr CustomScriptVL () where
  iScr prop (Script _ ops) = do
    mapM_ (iScr prop) ops

-- Note: doesn't do checks on existing variables for now
instance IScr ScriptOpVL () where
  iScr prop (DeclareVar varDef) = do
    IScrRuntime varsRef <- ask
    vars <- liftIO $ readIORef varsRef

    -- N.B.: this is not particular extensible. Only PoC
    case varDef of
      BoolVar name defBoolVal -> do
        varRef <- liftIO $ newIORef $ unsafeCoerce defBoolVal
        let vars' = Map.insert name varRef vars
        liftIO $ writeIORef varsRef vars'

  iScr prop (WriteData target source) = do
    readWrite prop source target

readWrite
  :: DMod.Property
  -> SourceVL typeTag
  -> TargetVL typeTag
  -> ScriptInterpreter ()
readWrite prop (FromVar fromVarDef) (ToVar toVarDef) = do
  IScrRuntime varsRef <- ask
  vars <- liftIO $ readIORef varsRef

  case (fromVarDef, toVarDef) of
    (BoolVar from _, BoolVar to _) -> do
      case (Map.lookup from vars, Map.lookup to vars) of
        (Nothing, _)  -> error $ show $ "From var not found: " <> from
        (_, Nothing)  -> error $ show $ "To var not found: " <> to
        (Just fromRef, Just toRef) -> liftIO $ do
          val <- readIORef fromRef
          writeIORef toRef val
    _ -> error $ show "Read/Write is not implemented or type mismatch."

makeIScrRuntime :: IO IScrRuntime
makeIScrRuntime = do
  varsRef <- newIORef Map.empty
  pure $ IScrRuntime varsRef

clearRuntime :: IScrRuntime -> IO ()
clearRuntime (IScrRuntime varsRef) = writeIORef varsRef Map.empty

makeScript
  :: DMod.Property
  -> PropertyScriptVL
  -> IO (DMod.Essence, DMod.DynamicScript)
makeScript prop (PropScript (Ess ess) customScr) = do
  runtime <- makeIScrRuntime

  -- TODO: bracket pattern and error handling
  let ioAct = do
        putStrLn $ "Running script: " <> ess
        runReaderT (iScr prop customScr) runtime
        clearRuntime runtime
        putStrLn $ "Script finished: " <> ess
  pure (ess, DMod.DynScript ioAct)

