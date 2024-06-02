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
  BoolVar :: Symbol -> VarDef BoolTag varName
  -- ^ type-level bool representation
  BoolVarVL :: String -> VarDef BoolTag varName
  -- ^ value-level bool representation

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

data Func typeTag where
  Negate :: Func BoolTag

-- | Script type
data CustomScript (lvl :: Level) where
  Script
    :: StringType lvl
    -- ^ Description
    -> [ScriptOp]
    -> CustomScript lvl


type CustomScriptTL = CustomScript 'TypeLevel
type CustomScriptVL = CustomScript 'ValueLevel

