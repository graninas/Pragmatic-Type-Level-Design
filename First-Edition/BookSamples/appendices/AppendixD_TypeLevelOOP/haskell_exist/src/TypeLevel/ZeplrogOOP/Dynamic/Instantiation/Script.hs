{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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

type ScriptInterpreter = ReaderT IScrRuntime IO ()

class IScr it where
  iScr :: DMod.Property -> it -> ScriptInterpreter

instance IScr CustomScriptVL where
  iScr prop (Script _ ops) = do
    mapM_ (iScr prop) ops

-- Note: doesn't do checks on existing variables for now
instance IScr ScriptOp where
  iScr prop (DeclareVar varDef) = do
    IScrRuntime varsRef <- ask
    vars <- liftIO $ readIORef varsRef

    case varDef of
      BoolVarVL name defBoolVal -> do
        varRef <- liftIO $ newIORef $ unsafeCoerce defBoolVal
        let vars' = Map.insert name varRef vars
        liftIO $ writeIORef varsRef vars'

  -- DeclareVar :: VarDef typeTag -> ScriptOp
  -- WriteVar   :: VarDef typeTag -> EssencePathTL -> ScriptOp
  -- QueryVal   :: EssencePathTL -> ToVarAct typeTag -> ScriptOp
  -- Invoke
  --   :: Func typeTag
  --   -> VarDef typeTag
  --   -> ToVarAct typeTag
  --   -> ScriptOp


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
        runReaderT (iScr prop customScr) runtime
        clearRuntime runtime
  pure (ess, DMod.DynScript ioAct)

