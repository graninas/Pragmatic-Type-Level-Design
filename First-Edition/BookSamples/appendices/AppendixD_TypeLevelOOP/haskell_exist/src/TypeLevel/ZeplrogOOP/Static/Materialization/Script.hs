{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TypeLevel.ZeplrogOOP.Static.Materialization.Script where

import CPrelude

import TypeLevel.ZeplrogOOP.Static.Model
import TypeLevel.ZeplrogOOP.Static.Materialization.Materializer
import TypeLevel.ZeplrogOOP.Static.Materialization.Common

import TypeSelector.Granular
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map



data ScrOps ops

instance SMat () 'True Bool where
  sMat () _ = pure True

instance SMat () 'False Bool where
  sMat () _ = pure False

instance
  ( KnownSymbol varName
  , SMat () defVal Bool
  ) =>
  SMat () ('BoolVar @'TypeLevel varName defVal)
          (VarDefVL BoolTag) where
  sMat () _ = do
    let varName = symbolVal $ Proxy @varName
    defVal <- sMat () $ Proxy @defVal
    pure $ BoolVar varName defVal

instance
  ( varDef ~ (vd :: VarDefTL typeTag)
  , SMat () varDef (VarDefVL typeTag)
  ) =>
  SMat () ('ToVar @'TypeLevel varDef)
          (TargetVL typeTag) where
  sMat () _ = do
    varDef <- sMat () $ Proxy @varDef
    pure $ ToVar varDef

instance
  ( SMat () (Essences essPath) [EssenceVL]
  , proxy ~ (p :: Proxy typeTag)
  ) =>
  SMat () ('ToField proxy essPath)
          (TargetVL typeTag) where
  sMat () _ = do
    path <- sMat () $ Proxy @(Essences essPath)
    pure $ ToField (Proxy @typeTag) path

instance
  ( varDef ~ (vd :: VarDefTL typeTag)
  , SMat () varDef (VarDefVL typeTag)
  ) =>
  SMat () ('FromVar @'TypeLevel varDef)
          (SourceVL typeTag) where
  sMat () _ = do
    varDef <- sMat () $ Proxy @varDef
    pure $ FromVar varDef

instance
  ( SMat () (Essences essPath) [EssenceVL]
  , proxy ~ (p :: Proxy typeTag)
  ) =>
  SMat () ('FromField proxy essPath)
          (SourceVL typeTag) where
  sMat () _ = do
    path <- sMat () $ Proxy @(Essences essPath)
    pure $ FromField (Proxy @typeTag) path

instance
  SMat () ('Negate @'TypeLevel)
          (FuncVL BoolTag) where
  sMat () _ = pure Negate

instance
  ( SMat () varDef (VarDefVL typeTag)
  ) =>
  SMat () ('DeclareVar @'TypeLevel varDef)
         ScriptOpVL where
  sMat () _ = do
    varDef <- sMat () $ Proxy @varDef
    pure $ DeclareVar varDef

instance
  ( SMat () source (SourceVL typeTag)
  , SMat () target (TargetVL typeTag)
  ) =>
  SMat () ('WriteData @'TypeLevel target source)
         ScriptOpVL where
  sMat () _ = do
    source <- sMat () $ Proxy @source
    target <- sMat () $ Proxy @target
    pure $ WriteData target source

instance
  ( SMat () source (SourceVL typeTag)
  , SMat () target (TargetVL typeTag)
  ) =>
  SMat () ('ReadData @'TypeLevel source target)
         ScriptOpVL where
  sMat () _ = do
    source <- sMat () $ Proxy @source
    target <- sMat () $ Proxy @target
    pure $ ReadData source target

instance
  ( SMat () func   (FuncVL typeTag)
  , SMat () varDef (VarDefVL typeTag)
  , SMat () target (TargetVL typeTag)
  ) =>
  SMat () ('Invoke @'TypeLevel func varDef target)
         ScriptOpVL where
  sMat () _ = do
    func   <- sMat () $ Proxy @func
    varDef <- sMat () $ Proxy @varDef
    target <- sMat () $ Proxy @target
    pure $ Invoke func varDef target

instance
  SMat () (ScrOps '[]) [ScriptOpVL] where
  sMat () _ = pure []

instance
  ( SMat () op ScriptOpVL
  , SMat () (ScrOps ops) [ScriptOpVL]
  ) =>
  SMat () (ScrOps (op ': ops)) [ScriptOpVL] where
  sMat () _ = do
    op  <- sMat () $ Proxy @op
    ops <- sMat () $ Proxy @(ScrOps ops)
    pure $ op : ops

instance
  ( KnownSymbol descr
  , SMat () (ScrOps ops) [ScriptOpVL]
  ) =>
  SMat () ('Script @'TypeLevel descr ops)
         CustomScriptVL where
  sMat () _ = do
    descr <- sMat () $ Proxy @descr
    ops   <- sMat () $ Proxy @(ScrOps ops)
    pure $ Script descr ops


