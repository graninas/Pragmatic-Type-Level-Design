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

instance
  IScr EssenceVL DMod.Essence where
  iScr _ (Ess ess) = pure ess

-- N.B., this is not quite extensible.
-- Also, duplicates DInst instance
instance IScr ValDefVL DMod.Value where
  iScr _ (IntValue val)    = pure $ DMod.IntValue val
  iScr _ (BoolValue val)   = pure $ DMod.BoolValue val
  iScr _ (StringValue val) = pure $ DMod.StringValue val
  iScr prop (TagValue tagProp cVal) = do
    val <- iScr prop cVal
    pure $ DMod.TagValue tagProp val
  iScr prop (PairValue val1 val2) = do
    val1' <- iScr prop val1
    val2' <- iScr prop val2
    pure $ DMod.PairValue val1' val2'
  iScr prop (PathValue essPath) = do
    essPath' <- mapM (iScr prop) essPath
    pure $ DMod.PathValue essPath'

instance IScr ScriptOpVL () where
  iScr prop (DeclareVar varDef) = do
    IScrRuntime varsRef <- ask
    vars <- liftIO $ readIORef varsRef

    case varDef of
      GenericVar name defVal typeName -> do
        val    <- iScr prop defVal
        varRef <- liftIO $ newIORef $ unsafeCoerce val

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
invokeF (Just NegateF) anyVal = let
  val :: DMod.Value = unsafeCoerce anyVal
  in case val of
        DMod.BoolValue b -> unsafeCoerce $ DMod.BoolValue $ not b
        _ -> error $ "invokeF (Just NegateF) type mismatch: " <> show val

readWrite
  :: DMod.Property
  -> Maybe (FuncVL typeTag1 typeTag2)
  -> SourceVL typeTag1
  -> TargetVL typeTag2
  -> ScriptInterpreter ()
readWrite prop mbF
  (FromVar (GenericVar from _ typeName1))
  (ToVar   (GenericVar to   _ typeName2))
    | typeName1 /= typeName2 = error $ show
            $ "readWrite (FromVar, ToVar) type mismatch: "
            <> from <> "(" <> typeName1 <> ")"
            <> to <> "(" <> typeName2 <> ")"

    | otherwise = do

  IScrRuntime varsRef <- ask
  vars <- liftIO $ readIORef varsRef

  case (Map.lookup from vars, Map.lookup to vars) of
    (Nothing, _) -> error $ show $ "readWrite (FromVar, ToVar) from var not found: " <> from
    (_, Nothing) -> error $ show $ "readWrite (FromVar, ToVar) to var not found: " <> to
    (Just fromRef, Just toRef) -> liftIO $ do
      val <- readIORef fromRef
      let val' = invokeF mbF val
      writeIORef toRef val'

-- readWrite prop mbF
--   (FromVar (GenericVar from _ typeName1))
--   (ToField _ statPath) = do
--     IScrRuntime varsRef <- ask
--     vars   <- liftIO $ readIORef varsRef

--     let dynPath = DInst.toDynEssPath statPath

--     curValRef <- liftIO $ Q.queryValue prop dynPath
--     -- curVal    <- liftIO $ readIORef curValRef

--     liftIO $ case Map.lookup from vars of
--       Nothing      -> error $ show $ "readWrite (FromVar, ToField) from var not found: " <> from
--       Just fromRef -> do
--         anyVal1 <- readIORef fromRef
--         let anyVal2 = invokeF mbF anyVal1

--   -- ??????
--         writeIORef curValRef $ DMod.BoolValue $ unsafeCoerce anyVal2
--       -- (BoolVar from _, _)   -> error $ show $ "readWrite (FromVar, ToField) type mismatch (target is not bool): " <> from
--       -- (_, DMod.BoolValue _) -> error $ "readWrite (FromVar, ToField) type mismatch (source is not bool)"

-- readWrite prop mbF
--   (FromField _ statPath)
--   (ToVar (GenericVar to _ typeName2)) = do
--     IScrRuntime varsRef <- ask
--     vars   <- liftIO $ readIORef varsRef

--     let dynPath = DInst.toDynEssPath statPath

--     curValRef <- liftIO $ Q.queryValue prop dynPath
--     curVal    <- liftIO $ readIORef curValRef

--     case Map.lookup to vars of
--       Nothing    -> error $ show $ "readWrite (FromField, ToVar) To var not found: " <> to
--       Just toRef -> do
--         -- ??????
--         let boolVal = True

--         let anyVal2 = invokeF mbF $ unsafeCoerce boolVal
--         writeIORef toRef $ unsafeCoerce anyVal2

-- readWrite prop mbF
--   (FromField _ fromStatPath)
--   (ToField _ toStatPath) = do
--     let fromDynPath = DInst.toDynEssPath fromStatPath
--     let toDynPath   = DInst.toDynEssPath toStatPath

--     fromValRef <- liftIO $ Q.queryValue prop fromDynPath
--     fromVal    <- liftIO $ readIORef fromValRef

--     toValRef <- liftIO $ Q.queryValue prop toDynPath
--     toVal    <- liftIO $ readIORef toValRef

--     -- liftIO $ case (fromVal, toVal) of
--     --   (DMod.BoolValue boolVal, DMod.BoolValue _) -> do
--     --     let anyVal = invokeF mbF $ unsafeCoerce boolVal
--     --     writeIORef toValRef $ DMod.BoolValue $ unsafeCoerce anyVal
--     --   (DMod.BoolValue _, _) -> error $ show $ "readWrite (FromField, ToField) type mismatch (target is not bool)"
--     --   (_, DMod.BoolValue _) -> error $ "readWrite (FromField, ToField) type mismatch (source is not bool)"
--     --   _                     -> error "readWrite (FromField, ToField) not yet implemented"

-- readWrite prop mbF (FromConst constVal) (ToVar toVarDef) = do
--   IScrRuntime varsRef <- ask
--   vars   <- liftIO $ readIORef varsRef

--   -- N.B.: this is not particular extensible. Only PoC
--   liftIO $ case (toVarDef, constVal) of
--     (BoolVar to _, BoolConst boolVal) -> do
--       case Map.lookup to vars of
--         Nothing    -> error $ show $ "To var not found: " <> to
--         Just toRef -> do
--           let anyVal = invokeF mbF $ unsafeCoerce boolVal
--           writeIORef toRef anyVal
--     _ -> error "readWrite (FromField, ToVar) not yet implemented"

-- readWrite prop mbF (FromConst constVal) (ToField _ toStatPath) = do
--   let toDynPath = DInst.toDynEssPath toStatPath

--   toValRef <- liftIO $ Q.queryValue prop toDynPath
--   toVal    <- liftIO $ readIORef toValRef

--   -- N.B.: this is not particular extensible. Only PoC
--   liftIO $ case (constVal, toVal) of
--     (BoolConst boolVal, DMod.BoolValue _) -> do
--       let anyVal = invokeF mbF $ unsafeCoerce boolVal
--       writeIORef toValRef $ DMod.BoolValue $ unsafeCoerce anyVal
--     _ -> error "readWrite (FromField, ToField) not yet implemented"



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

