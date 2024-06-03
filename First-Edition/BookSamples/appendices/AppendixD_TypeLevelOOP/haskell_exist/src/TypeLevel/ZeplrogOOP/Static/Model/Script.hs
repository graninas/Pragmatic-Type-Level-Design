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

data VarDef (lvl :: Level) typeTag where
  BoolVar :: StringType lvl -> Bool -> VarDef lvl BoolTag

data ScriptOp (lvl :: Level) where
  DeclareVar   :: VarDef lvl typeTag -> ScriptOp lvl
  ReadData     :: Source lvl typeTag -> Target lvl typeTag -> ScriptOp lvl
  WriteData    :: Target lvl typeTag -> Source lvl typeTag -> ScriptOp lvl
  Invoke
    :: Func lvl typeTag
    -> VarDef lvl typeTag
    -> Target lvl typeTag
    -> ScriptOp lvl

data Target (lvl :: Level) typeTag where
  ToField :: Proxy typeTag -> EssencePath lvl -> Target lvl typeTag
  ToVar   :: VarDef lvl typeTag -> Target lvl typeTag

data Source (lvl :: Level) typeTag where
  FromField :: Proxy typeTag -> EssencePath lvl -> Source lvl typeTag
  FromVar   :: VarDef lvl typeTag -> Source lvl typeTag

data Func (lvl :: Level) typeTag where
  Negate :: Func lvl BoolTag

-- | Script type
data CustomScript (lvl :: Level) where
  Script
    :: StringType lvl
    -- ^ Description
    -> [ScriptOp lvl]
    -> CustomScript lvl


type CustomScriptTL = CustomScript 'TypeLevel
type CustomScriptVL = CustomScript 'ValueLevel

type FuncTL = Func 'TypeLevel
type FuncVL = Func 'ValueLevel

type SourceTL = Source 'TypeLevel
type SourceVL = Source 'ValueLevel

type TargetTL = Target 'TypeLevel
type TargetVL = Target 'ValueLevel

type VarDefTL = VarDef 'TypeLevel
type VarDefVL = VarDef 'ValueLevel

type ScriptOpTL = ScriptOp 'TypeLevel
type ScriptOpVL = ScriptOp 'ValueLevel

