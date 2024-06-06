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
import qualified TypeLevel.ZeplrogOOP.Dynamic.Instantiation.Common as DInst
import qualified TypeLevel.ZeplrogOOP.Dynamic.Query as Q

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

  iScr prop (WriteData target source) =
    readWrite prop Nothing source target

  iScr prop (ReadData source target) =
    readWrite prop Nothing source target

  iScr prop (Invoke func source target) =
    readWrite prop (Just func) source target


invokeF
  :: Maybe (FuncVL typeTag1 typeTag2)
  -> GHC.Any
  -> GHC.Any
invokeF Nothing anyVal = anyVal
invokeF (Just NegateF) anyVal =
  unsafeCoerce $ not $ unsafeCoerce anyVal


-- N.B.: badly written code.
readWrite
  :: DMod.Property
  -> Maybe (FuncVL typeTag1 typeTag2)
  -> SourceVL typeTag1
  -> TargetVL typeTag2
  -> ScriptInterpreter ()
readWrite prop mbF (FromVar fromVarDef) (ToVar toVarDef) = do
  IScrRuntime varsRef <- ask
  vars <- liftIO $ readIORef varsRef

  -- N.B.: this is not particular extensible. Only PoC
  case (fromVarDef, toVarDef) of
    (BoolVar from _, BoolVar to _) ->
      case (Map.lookup from vars, Map.lookup to vars) of
        (Nothing, _)  -> error $ show $ "From var not found: " <> from
        (_, Nothing)  -> error $ show $ "To var not found: " <> to
        (Just fromRef, Just toRef) -> liftIO $ do
          val <- readIORef fromRef
          let val' = invokeF mbF val
          writeIORef toRef val'
    _ -> error $ show "Read/Write is not implemented or type mismatch."

readWrite prop mbF (FromVar fromVarDef) (ToField _ statPath) = do
  IScrRuntime varsRef <- ask
  vars   <- liftIO $ readIORef varsRef

  let dynPath = DInst.toDynEssPath statPath

  curValRef <- liftIO $ Q.queryValue prop dynPath
  curVal    <- liftIO $ readIORef curValRef

  -- N.B.: this is not particular extensible. Only PoC
  liftIO $ case (fromVarDef, curVal) of
    (BoolVar from _, DMod.BoolValue _) ->
      case Map.lookup from vars of
        Nothing      -> error $ show $ "From var not found: " <> from
        Just fromRef -> do
          anyVal1 <- readIORef fromRef
          let anyVal2 = invokeF mbF anyVal1
          writeIORef curValRef $ DMod.BoolValue $ unsafeCoerce anyVal2
    (BoolVar from _, _)   -> error $ show $ "readWrite (FromVar, ToField) type mismatch (target is not bool): " <> from
    (_, DMod.BoolValue _) -> error $ "readWrite (FromVar, ToField) type mismatch (source is not bool)"
    _                     -> error "readWrite (FromVar, ToField) not yet implemented"

readWrite prop mbF (FromField _ statPath) (ToVar toVarDef) = do
  IScrRuntime varsRef <- ask
  vars   <- liftIO $ readIORef varsRef

  let dynPath = DInst.toDynEssPath statPath

  curValRef <- liftIO $ Q.queryValue prop dynPath
  curVal    <- liftIO $ readIORef curValRef

  -- N.B.: this is not particular extensible. Only PoC
  liftIO $ case (toVarDef, curVal) of
    (BoolVar to _, DMod.BoolValue boolVal) ->
      case Map.lookup to vars of
        Nothing    -> error $ show $ "To var not found: " <> to
        Just toRef -> do
          let anyVal2 = invokeF mbF $ unsafeCoerce boolVal
          writeIORef toRef $ unsafeCoerce anyVal2
    (BoolVar to _, _)     -> error $ show $ "readWrite (FromField, ToVar) type mismatch (source is not bool): " <> to
    (_, DMod.BoolValue _) -> error $ "readWrite (FromField, ToVar) type mismatch (target is not bool)"
    _                     -> error "readWrite (FromField, ToVar) not yet implemented"

readWrite prop mbF (FromField _ fromStatPath) (ToField _ toStatPath) = do
  let fromDynPath = DInst.toDynEssPath fromStatPath
  let toDynPath   = DInst.toDynEssPath toStatPath

  fromValRef <- liftIO $ Q.queryValue prop fromDynPath
  fromVal    <- liftIO $ readIORef fromValRef

  toValRef <- liftIO $ Q.queryValue prop toDynPath
  toVal    <- liftIO $ readIORef toValRef

  -- N.B.: this is not particular extensible. Only PoC
  liftIO $ case (fromVal, toVal) of
    (DMod.BoolValue boolVal, DMod.BoolValue _) -> do
      let anyVal = invokeF mbF $ unsafeCoerce boolVal
      writeIORef toValRef $ DMod.BoolValue $ unsafeCoerce anyVal
    (DMod.BoolValue _, _) -> error $ show $ "readWrite (FromField, ToField) type mismatch (target is not bool)"
    (_, DMod.BoolValue _) -> error $ "readWrite (FromField, ToField) type mismatch (source is not bool)"
    _                     -> error "readWrite (FromField, ToField) not yet implemented"

readWrite prop mbF (FromConst constVal) (ToVar toVarDef) = do
  IScrRuntime varsRef <- ask
  vars   <- liftIO $ readIORef varsRef

  -- N.B.: this is not particular extensible. Only PoC
  liftIO $ case (toVarDef, constVal) of
    (BoolVar to _, BoolConst boolVal) -> do
      case Map.lookup to vars of
        Nothing    -> error $ show $ "To var not found: " <> to
        Just toRef -> do
          let anyVal = invokeF mbF $ unsafeCoerce boolVal
          writeIORef toRef anyVal
    _ -> error "readWrite (FromField, ToVar) not yet implemented"

readWrite prop mbF (FromConst constVal) (ToField _ toStatPath) = do
  let toDynPath = DInst.toDynEssPath toStatPath

  toValRef <- liftIO $ Q.queryValue prop toDynPath
  toVal    <- liftIO $ readIORef toValRef

  -- N.B.: this is not particular extensible. Only PoC
  liftIO $ case (constVal, toVal) of
    (BoolConst boolVal, DMod.BoolValue _) -> do
      let anyVal = invokeF mbF $ unsafeCoerce boolVal
      writeIORef toValRef $ DMod.BoolValue $ unsafeCoerce anyVal
    _ -> error "readWrite (FromField, ToField) not yet implemented"



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

