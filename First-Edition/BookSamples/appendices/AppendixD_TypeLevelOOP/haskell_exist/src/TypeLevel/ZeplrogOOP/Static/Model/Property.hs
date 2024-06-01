{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Static property model.
-- Can work as a static type-level or static value-level model.

module TypeLevel.ZeplrogOOP.Static.Model.Property where

import CPrelude

import TypeSelector.Granular
import TypeLevel.ZeplrogOOP.Static.Model.Common
import TypeLevel.ZeplrogOOP.Static.Model.Script

import GHC.TypeLits
import qualified Data.Map as Map


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

-- | Used to make static property hierarchies.
data PropertyGroup (lvl :: Level) where
  -- | Property groups for static type-level representation.
  Group     :: EssenceTL -> PropertyGroupTL
  GroupRoot :: EssenceTL -> PropertyTL -> PropertyGroupTL

  -- | Property groups and id for static value-level representation.
  GroupId      :: EssenceVL -> StaticPropertyId -> PropertyGroupVL
  GroupRootId  :: EssenceVL -> StaticPropertyId -> PropertyVL -> PropertyGroupVL

-- | Property owning
data PropertyOwning (lvl :: Level) where
  -- | Own value. Will be materialized for each parent prop.
  OwnVal     :: ValDef lvl -> PropertyOwning lvl
  -- | Own property. Will be materialized for each parent prop.
  OwnProp    :: Property lvl -> PropertyOwning lvl
  -- | Shared property. Will be materialized only once and shared between parents.
  SharedProp :: Property lvl -> PropertyOwning lvl

-- | Key-value pair for a property
data PropertyKeyValue (lvl :: Level) where
  -- | Implicit dictionary of properties.
  PropKeyBag :: Essence lvl -> [Property lvl] -> PropertyKeyValue lvl
  -- | Separate property
  PropKeyVal :: Essence lvl -> PropertyOwning lvl -> PropertyKeyValue lvl

data PropertyScript where
  PropScript :: EssenceTL -> Script -> PropertyScript

  -- | Abstract property.
  --   Provides the shape for the derived properties.
--     Abstract property can't be value level.
data AbstractProperty where
  -- | Regular abstract property with fields.
  AbstractProp
    :: PropertyGroupTL
    -> [PropertyKeyValueTL]
    -> [PropertyScript]
    -> AbstractProperty

-- | Static property that must be stat and dyn materialized.
data Property (lvl :: Level) where
  -- | Tag property reference.
  TagPropRef
    :: TagProperty lvl
    -> Property lvl

  -- | Derived property from any abstract property.
  --    Will take the shape of the parent.
  --    After static materialization, becomes PropDict.
  --    DeriviedProp can't be value level.
  DerivedProp
    :: EssenceTL
    -> AbstractProperty
    -> [PropertyKeyValueTL]
    -> [PropertyScript]
    -> PropertyTL

  -- | Compound property for static value-level and dynamic
  -- value-level representation.
  -- Can't be type-level; use AbstractProp and DerivedProp instead.
  PropDict
    :: PropertyGroupVL
    -> [PropertyKeyValueVL]
    -> Map.Map EssenceVL String  --tODO
    -> PropertyVL

------ Short identifiers ----------
type TagPropertyGroupTL = TagPropertyGroup 'TypeLevel
type TagPropertyGroupVL = TagPropertyGroup 'ValueLevel

type TagPropertyTL = TagProperty 'TypeLevel
type TagPropertyVL = TagProperty 'ValueLevel

type PropertyGroupTL = PropertyGroup 'TypeLevel
type PropertyGroupVL = PropertyGroup 'ValueLevel

type PropertyTL = Property 'TypeLevel
type PropertyVL = Property 'ValueLevel

type PropertyKeyValueTL = PropertyKeyValue 'TypeLevel
type PropertyKeyValueVL = PropertyKeyValue 'ValueLevel

type PropertyOwningTL = PropertyOwning 'TypeLevel
type PropertyOwningVL = PropertyOwning 'ValueLevel

