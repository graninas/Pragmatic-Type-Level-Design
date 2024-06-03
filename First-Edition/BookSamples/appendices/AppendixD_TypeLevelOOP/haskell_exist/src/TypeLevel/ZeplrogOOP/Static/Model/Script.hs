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
  DeclareVar   :: VarDef lvl typeTag -> ScriptOp
  Read         :: Source lvl typeTag -> Target lvl typeTag -> ScriptOp lvl
  Write        :: Source lvl typeTag -> Target lvl typeTag -> ScriptOp lvl
  Invoke
    :: Func typeTag
    -> VarDef lvl typeTag
    -> Target lvl typeTag
    -> ScriptOp lvl

data Target (lvl :: Level) typeTag where
  ToField :: EssencePath lvl -> Target lvl typeTag
  ToVar   :: VarDef lvl typeTag -> Target lvl typeTag

data Source (lvl :: Level) typeTag where
  FromField :: EssencePath lvl -> Source lvl typeTag
  FromVar   :: VarDef lvl typeTag -> Source lvl typeTag

data Func typeTag where
  Negate :: Func BoolTag

-- | Script type
data CustomScript (lvl :: Level) where
  Script
    :: StringType lvl
    -- ^ Description
    -> [ScriptOp lvl]
    -> CustomScript lvl


type CustomScriptTL = CustomScript 'TypeLevel
type CustomScriptVL = CustomScript 'ValueLevel

