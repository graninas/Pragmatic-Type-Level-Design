{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module TypeLevel.ZeplrogOOP.Static.Model.Script where

import CPrelude

import TypeSelector.Granular
import TypeLevel.ZeplrogOOP.Static.Model.Common

import GHC.TypeLits


-- | Variable definition
data VarDef (lvl :: Level) typeTag where
  GenericVar
    :: StringType lvl       -- ^ Var name
    -> ValDef lvl           -- ^ Default value
    -> StringType lvl       -- ^ Stringified type name
    -> VarDef lvl typeTag

-- | Constant definition
data ConstDef (lvl :: Level) typeTag where
  GenericConst
    :: ValDef lvl
    -> StringType lvl       -- ^ Stringified type name
    -> ConstDef lvl typeTag

-- | Script operation
data ScriptOp (lvl :: Level) where
  DeclareVar   :: VarDef lvl typeTag -> ScriptOp lvl

  -- Can be the only MOV instruction
  ReadData
    :: Source lvl typeTag
    -> Target lvl typeTag
    -> ScriptOp lvl
  WriteData
    :: Target lvl typeTag
    -> Source lvl typeTag
    -> ScriptOp lvl

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
  NegateF :: Func lvl BoolTag BoolTag

-- | Script type
data CustomScript (lvl :: Level) where
  Script
    :: StringType lvl
    -- ^ Description
    -> [ScriptOp lvl]
    -> CustomScript lvl

-- Predefined var types

type IntVar (name :: Symbol) (i :: Nat)
  = GenericVar @'TypeLevel @IntTag name (IntValue i) IntTag

type BoolVar (name :: Symbol) (b :: Bool)
  = GenericVar @'TypeLevel @BoolTag name (BoolValue b) BoolTag

type StringVar (name :: Symbol) (s :: Symbol)
  = GenericVar @'TypeLevel @StringTag name (StringValue s) StringTag

type PathVar (name :: Symbol) (s :: [EssenceTL])
  = GenericVar @'TypeLevel @PathTag name (PathValue s) PathTag

-- TODO: rest of vars

-- Predefined const types

type IntConst (i :: Nat)
  = GenericConst @'TypeLevel @IntTag (IntValue i) IntTag

type BoolConst (b :: Bool)
  = GenericConst @'TypeLevel @BoolTag (BoolValue b) BoolTag

type StringConst (s :: Symbol)
  = GenericConst @'TypeLevel @StringTag (StringValue s) StringTag

type PathConst (s :: [EssenceTL])
  = GenericConst @'TypeLevel @PathTag (PathValue s) PathTag

-- TODO: rest of consts

-- Short definitions

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

