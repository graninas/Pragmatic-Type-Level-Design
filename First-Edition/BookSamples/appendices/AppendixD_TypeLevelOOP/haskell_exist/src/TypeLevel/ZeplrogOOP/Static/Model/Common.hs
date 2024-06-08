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

-- | Real Id of a property (for value-level usage only)

newtype StaticPropertyId = StaticPropertyId Int
  deriving (Show, Eq, Ord)

-- | Tag property is always static.
--   Used to tag and group notions.
--   Can be hierarchical.
data TagPropertyGroup (lvl :: Level) where
  -- | Tag property groups for static type-level representation.
  TagGroup     :: Essence lvl -> TagPropertyGroup lvl
  TagGroupRoot :: Essence lvl -> TagProperty lvl -> TagPropertyGroup lvl

-- | Tag property: immutable, reference-only,
--   one instance, only for grouping.
data TagProperty (lvl :: Level) where
  -- | Tag prop for static type-level and
  --   dynamic value-level representation.
  TagProp
    :: TagPropertyGroup lvl
    -> TagProperty lvl

-- | Value definition with a default value

data ValDef (lvl :: Level) where
  IntValue      :: IntegerType lvl -> ValDef lvl
  BoolValue     :: Bool -> ValDef lvl
  PairValue     :: ValDef lvl -> ValDef lvl-> ValDef lvl
  StringValue   :: StringType lvl-> ValDef lvl

  -- | Reference to a dynamic property relative to the parent prop
  PathValue     :: [Essence lvl] -> ValDef lvl

  -- | Reference to a tag property
  -- with a value
  TagValue      :: TagProperty lvl -> ValDef lvl -> ValDef lvl


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
