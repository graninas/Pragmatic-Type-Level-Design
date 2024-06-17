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
  StringValue   :: StringType lvl-> ValDef lvl
  PairValue     :: ValDef lvl -> ValDef lvl-> ValDef lvl

  -- TODO:
  -- ListValue     :: [ValDef lvl] -> ValDef lvl
  -- EssenceValue  :: Essence lvl -> ValDef lvl


  -- | Reference to a dynamic property relative to the parent prop
  PathValue     :: [Essence lvl] -> ValDef lvl

  -- | Reference to a tag property
  -- with a value
  TagValue      :: TagProperty lvl -> ValDef lvl -> ValDef lvl

  -- | Value should be overridden.
  --   Provides default value.
  DerivableValue  :: ValDef lvl -> ValDef lvl

type IntTag    = "tag:int"
type BoolTag   = "tag:bool"
type StringTag = "tag:string"
type PairTag   = "tag:pair"
type PathTag   = "tag:path"
type TagTag    = "tag:tag"

------ Short identifiers ----------

type EssenceTL = Essence 'TypeLevel
type EssenceVL = Essence 'ValueLevel

type TagPropertyGroupTL = TagPropertyGroup 'TypeLevel
type TagPropertyGroupVL = TagPropertyGroup 'ValueLevel

type TagPropertyTL = TagProperty 'TypeLevel
type TagPropertyVL = TagProperty 'ValueLevel

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

instance Eq TagPropertyGroupVL where
  (==) (TagGroup a) (TagGroup b) = a == b
  (==) (TagGroupRoot a l) (TagGroupRoot b r) = (a == b) && (l == r)
  (==) _ _ = False

instance Ord TagPropertyGroupVL where
  compare (TagGroup a) (TagGroup b) = compare a b
  compare (TagGroupRoot a l) (TagGroupRoot b r)
    = compare (compare a b) (compare l r)
  compare (TagGroup _) (TagGroupRoot _ _) = GT
  compare _ _ = LT

instance T.Show TagPropertyGroupVL where
  show (TagGroup a) = "TagGroup " <> T.show a
  show (TagGroupRoot a l)
    = "TagGroupRoot " <> T.show a <> " " <> T.show l

instance Eq TagPropertyVL where
  (==) (TagProp a) (TagProp b) = a == b

instance Ord TagPropertyVL where
  compare (TagProp a) (TagProp b) = compare a b

instance T.Show TagPropertyVL where
  show (TagProp a) = "TagProp " <> T.show a

