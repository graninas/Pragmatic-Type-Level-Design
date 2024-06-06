{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module TypeLevel.ZeplrogOOP.Static.Model.Script where

import CPrelude

import TypeSelector.Granular
import TypeLevel.ZeplrogOOP.Static.Model.Common

import GHC.TypeLits


data BoolTag = BoolTag
data StringTag = StringTag

-- | Variable definition
-- N.B., incomplete for now (PoC only)
data VarDef (lvl :: Level) typeTag where
  -- TODO
  -- BoolVar :: StringType lvl -> ValDef lvl typeTag -> VarDef lvl typeTag
  BoolVar   :: StringType lvl -> Bool -> VarDef lvl BoolTag
  StringVar :: StringType lvl -> StringType lvl -> VarDef lvl StringTag

-- | Constant definition
-- N.B., incomplete for now (PoC only)
data ConstDef (lvl :: Level) typeTag where
  BoolConst :: Bool -> ConstDef lvl BoolTag

-- | Script operation
data ScriptOp (lvl :: Level) where
  DeclareVar   :: VarDef lvl typeTag -> ScriptOp lvl

  -- Can be the only MOV instruction
  ReadData     :: Source lvl typeTag -> Target lvl typeTag -> ScriptOp lvl
  WriteData    :: Target lvl typeTag -> Source lvl typeTag -> ScriptOp lvl

  Invoke
    :: Func lvl typeTag1 typeTag2
    -> Source lvl typeTag1
    -> Target lvl typeTag2
    -> ScriptOp lvl

-- N.B., Proxy is only needed to satisfy functional dependency
-- that is somehow fails to define typeTag without this workaround.
data Target (lvl :: Level) typeTag where
  ToField :: Proxy typeTag -> EssencePath lvl -> Target lvl typeTag
  ToVar   :: VarDef lvl typeTag -> Target lvl typeTag

-- N.B., Proxy is only needed to satisfy functional dependency
-- that is somehow fails to define typeTag without this workaround.
data Source (lvl :: Level) typeTag where
  FromField :: Proxy typeTag -> EssencePath lvl -> Source lvl typeTag
  FromVar   :: VarDef lvl typeTag -> Source lvl typeTag
  FromConst :: ConstDef lvl typeTag -> Source lvl typeTag

-- | Function over a value
data Func (lvl :: Level) typeTag1 typeTag2 where
  NegateF    :: Func lvl BoolTag BoolTag

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

type ConstDefTL = ConstDef 'TypeLevel
type ConstDefVL = ConstDef 'ValueLevel

type ScriptOpTL = ScriptOp 'TypeLevel
type ScriptOpVL = ScriptOp 'ValueLevel

