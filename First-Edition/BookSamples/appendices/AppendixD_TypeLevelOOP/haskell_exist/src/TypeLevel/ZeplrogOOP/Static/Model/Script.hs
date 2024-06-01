{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module TypeLevel.ZeplrogOOP.Static.Model.Script where

import CPrelude

import TypeSelector.Granular
import TypeLevel.ZeplrogOOP.Static.Model.Common

import GHC.TypeLits


data BoolTag

data VarDef typeTag varName where
  BoolVar :: varName -> VarDef BoolTag varName

data ScriptOp where
  DeclareVar :: VarDef typeTag varName -> ScriptOp
  WriteVar   :: VarDef typeTag varName -> EssencePathTL -> ScriptOp
  QueryVal   :: EssencePathTL -> ToVarAct typeTag varName -> ScriptOp
  Invoke
    :: Func typeTag
    -> VarDef typeTag varName1
    -> ToVarAct typeTag varName2
    -> ScriptOp

data ToVarAct typeTag varName where
  ToVar :: VarDef typeTag varName -> ToVarAct typeTag varName

data Script = Script Symbol [ScriptOp]


data Func typeTag where
  Negate :: Func BoolTag
