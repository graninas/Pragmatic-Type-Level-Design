{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeLevel.ZeplrogOOP.Static.Model.Common where

import CPrelude

import TypeSelector.Granular

import GHC.TypeLits
import qualified Text.Show as T

------ Common and General -----------------

-- | Sudo ID of a property

data Essence (lvl :: Level) where
  Ess :: StringType lvl -> Essence lvl

type EssencePath (lvl :: Level) = [Essence lvl]

-- | Value definition with a default value

data ValDef (lvl :: Level) where
  IntValue      :: IntegerType lvl -> ValDef lvl
  BoolValue     :: Bool -> ValDef lvl
  PairValue     :: ValDef lvl -> ValDef  lvl-> ValDef lvl
  StringValue   :: StringType lvl-> ValDef lvl

  -- | Reference to a dynamic property relative to the parent prop
  PathValue     :: [Essence lvl] -> ValDef lvl

------ Short identifiers ----------

type EssenceTL = Essence 'TypeLevel
type EssenceVL = Essence 'ValueLevel

type ValDefTL = ValDef 'TypeLevel
type ValDefVL = ValDef 'ValueLevel

type EssencePathTL = EssencePath 'TypeLevel
type EssencePathVL = EssencePath 'ValueLevel

-------- Instances ------------------

instance Eq EssenceVL where
  (==) (Ess a) (Ess b) = a == b

instance Ord EssenceVL where
  compare (Ess a) (Ess b) = compare a b

instance T.Show EssenceVL where
  show (Ess a) = T.show a
