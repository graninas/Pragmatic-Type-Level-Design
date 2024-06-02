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


instance
  ( KnownSymbol varName
  ) =>
  SMat () ('BoolVar varName)
          (VarDef BoolTag varName) where
  sMat () _ = do
    let varName = symbolVal $ Proxy @varName
    pure $ BoolVarVL varName

instance
  ( SMat () varDef (VarDef typTag varName)
  ) =>
  SMat () ('ToVar varDef)
          (ToVarAct typeTag varName) where
  sMat () _ = do
    varDef <- sMat () $ Proxy @varDef
    pure $ ToVar varDef

instance
  SMat () 'Negate
          (Func BoolTag) where
  sMat () _ = pure Negate


instance
  ( SMat () varDef (VarDef typTag varName)
  ) =>
  SMat () ('DeclareVar varDef varName)
         ScriptOp where
  sMat () _ = do
    varDef <- sMat () $ Proxy @varDef
    pure $ DeclareVar varDef

instance
  ( SMat () varDef (VarDef typTag varName)
  , SMat () (Essences essPath) [EssenceVL]
  ) =>
  SMat () ('WriteVar varDef essPath)
         ScriptOp where
  sMat () _ = do
    varDef <- sMat () $ Proxy @varDef
    esss   <- sMat () $ Proxy @(Essences essPath)
    pure $ WriteVar varDef esss

instance
  ( SMat () toVarAct (ToVarAct typeTag varName)
  , SMat () (Essences essPath) [EssenceVL]
  ) =>
  SMat () ('QueryVal essPath toVarAct)
         ScriptOp where
  sMat () _ = do
    esss     <- sMat () $ Proxy @(Essences essPath)
    toVarAct <- sMat () $ Proxy @toVarAct
    pure $ QueryVal esss toVarAct

instance
  ( SMat () func (Func typeTag)
  , SMat () varDef (VarDef typeTag varName1)
  , SMat () toVarAct (ToVarAct typeTag varName2)
  ) =>
  SMat () ('Invoke func varDef toVarAct)
         ScriptOp where
  sMat () _ = do
    func     <- sMat () $ Proxy @func
    varDef   <- sMat () $ Proxy @varDef
    toVarAct <- sMat () $ Proxy @toVarAct
    pure $ Invoke func varDef toVarAct

instance
  SMat () (ScrOps '[]) [ScriptOp] where
  sMat () _ = pure []

instance
  ( SMat () op ScriptOp
  , SMat () (ScrOps ops) [ScriptOp]
  ) =>
  SMat () (ScrOps (op ': ops)) [ScriptOp] where
  sMat () _ = do
    op  <- sMat () $ Proxy @op
    ops <- sMat () $ Proxy @(ScrOps ops)
    pure $ op : ops

instance
  ( KnownSymbol descr
  , SMat () (ScrOps ops) [ScriptOp]
  ) =>
  SMat () ('Script descr ops)
         CustomScriptVL where
  sMat () _ = do
    descr <- sMat () $ Proxy @descr
    ops   <- sMat () $ Proxy @(ScrOps ops)
    pure $ CustomScript descr ops


