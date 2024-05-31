{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeLevel.ZeplrogOOP.Dynamic.Instantiation.Common where

import CPrelude

import qualified TypeLevel.ZeplrogOOP.Static.Model as SMod
import qualified TypeLevel.ZeplrogOOP.Static.Materialization as SMat
import TypeLevel.ZeplrogOOP.Dynamic.Instantiation.Instantiator
import TypeLevel.ZeplrogOOP.Dynamic.Model

import Data.Proxy
import System.Random
import qualified Data.Map.Strict as Map


-- Instantiate group and essence

instance
  DInst () SMod.EssenceVL Essence where
  dInst _ () (SMod.Ess ess) = pure ess

instance
  DInst () SMod.PropertyGroupVL (Essence, SMod.StaticPropertyId) where
  dInst _ () (SMod.GroupId statEss sId) = do
    ess <- dInst False () statEss
    pure (ess, sId)
  dInst _ () (SMod.GroupRootId statEss sId _) = do
    ess <- dInst False () statEss
    pure (ess, sId)

-- Instantiate value

instance
  DInst () SMod.ValDefVL Value where
  dInst _ () (SMod.IntValue val)  = pure $ IntValue val
  dInst _ () (SMod.BoolValue val) = pure $ BoolValue val
  dInst _ () (SMod.StringValue val) = pure $ StringValue val
  dInst _ () (SMod.PairValue val1 val2) = do
    val1' <- dInst False () val1
    val2' <- dInst False () val2
    pure $ PairValue (val1', val2')
  dInst _ () (SMod.PathValue essPath) = do
    essPath' <- mapM (dInst False ()) essPath
    pure $ Path essPath'

