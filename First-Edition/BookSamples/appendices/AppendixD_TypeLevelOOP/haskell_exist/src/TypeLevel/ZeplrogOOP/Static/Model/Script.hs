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

data VarDef typeTag where
  BoolVar :: Symbol -> Bool -> VarDef BoolTag
  -- ^ type-level bool representation
  BoolVarVL :: String -> Bool -> VarDef BoolTag
  -- ^ value-level bool representation

data ScriptOp where
  DeclareVar :: VarDef typeTag -> ScriptOp
  WriteVar   :: VarDef typeTag -> EssencePathTL -> ScriptOp
  QueryVal   :: EssencePathTL -> ToVarAct typeTag -> ScriptOp
  Invoke
    :: Func typeTag
    -> VarDef typeTag
    -> ToVarAct typeTag
    -> ScriptOp

data ToVarAct typeTag where
  ToVar :: VarDef typeTag -> ToVarAct typeTag

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

