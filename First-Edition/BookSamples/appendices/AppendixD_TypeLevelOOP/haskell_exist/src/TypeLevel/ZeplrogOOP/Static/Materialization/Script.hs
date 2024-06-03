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
  SMat () ('BoolVar varName defVal)
          (VarDef BoolTag) where
  sMat () _ = do
    let varName = symbolVal $ Proxy @varName
    defVal <- sMat () $ Proxy @defVal
    pure $ BoolVarVL varName defVal

instance
  ( varDef ~ (vd :: VarDef typeTag)
  , SMat () varDef (VarDef typeTag)
  ) =>
  SMat () ('ToVar varDef)
          (ToVarAct typeTag) where
  sMat () _ = do
    varDef <- sMat () $ Proxy @varDef
    pure $ ToVar varDef

instance
  SMat () 'Negate
          (Func BoolTag) where
  sMat () _ = pure Negate

instance
  ( SMat () varDef (VarDef typeTag)
  ) =>
  SMat () ('DeclareVar varDef)
         ScriptOp where
  sMat () _ = do
    varDef <- sMat () $ Proxy @varDef
    pure $ DeclareVar varDef

instance
  ( SMat () varDef (VarDef typeTag)
  , SMat () (Essences essPath) [EssenceVL]
  ) =>
  SMat () ('WriteVar varDef essPath)
         ScriptOp where
  sMat () _ = do
    varDef <- sMat () $ Proxy @varDef
    path   <- sMat () $ Proxy @(Essences essPath)
    pure $ WriteVarVL varDef path

instance
  ( SMat () toVarAct (ToVarAct typeTag)
  , SMat () (Essences essPath) [EssenceVL]
  ) =>
  SMat () ('QueryVal essPath toVarAct)
         ScriptOp where
  sMat () _ = do
    path     <- sMat () $ Proxy @(Essences essPath)
    toVarAct <- sMat () $ Proxy @toVarAct
    pure $ QueryValVL path toVarAct

instance
  ( SMat () func (Func typeTag)
  , SMat () varDef (VarDef typeTag)
  , SMat () toVarAct (ToVarAct typeTag)
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
  SMat () ('Script @'TypeLevel descr ops)
         CustomScriptVL where
  sMat () _ = do
    descr <- sMat () $ Proxy @descr
    ops   <- sMat () $ Proxy @(ScrOps ops)
    pure $ Script descr ops


