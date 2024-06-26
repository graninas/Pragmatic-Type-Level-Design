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

data PropertyScript (lvl :: Level) where
  PropScript
    :: Essence lvl
    -> CustomScript lvl
    -> PropertyScript lvl

  -- | Abstract property.
  --   Provides the shape for the derived properties.
--     Abstract property itself can't be value level.
data AbstractProperty where
  -- | Abstract property with fields.
  --   Becomes a regular static value-level property that can't be
  --   instantiated but can be referenced.
  --   Due to the group reference, can define properties in the middle of the hierarchy.
  AbstractProp
    :: PropertyGroupTL
    -> [PropertyKeyValueTL]
    -> [PropertyScriptTL]
    -> AbstractProperty
  -- | Abstract property that derives another abstract prop.
  --   Does not contain group because will not reference prop branches.
  --   Resulting PropDict will contain prop group with this essence only.
  AbstractDerivedProp
    :: EssenceTL
    -> AbstractProperty
    -> [PropertyKeyValueTL]
    -> [PropertyScriptTL]
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
    -> [PropertyScriptTL]
    -> PropertyTL

  -- | Compound property for static value-level and dynamic
  -- value-level representation.
  -- Can't be type-level; use AbstractProp and DerivedProp instead.
  PropDict
    :: PropertyGroupVL
    -> [PropertyKeyValueVL]
    -> [PropertyScriptVL]
    -> PropertyVL


------ Short identifiers ----------

type PropertyGroupTL = PropertyGroup 'TypeLevel
type PropertyGroupVL = PropertyGroup 'ValueLevel

type PropertyScriptTL = PropertyScript 'TypeLevel
type PropertyScriptVL = PropertyScript 'ValueLevel

type PropertyTL = Property 'TypeLevel
type PropertyVL = Property 'ValueLevel

type PropertyKeyValueTL = PropertyKeyValue 'TypeLevel
type PropertyKeyValueVL = PropertyKeyValue 'ValueLevel

type PropertyOwningTL = PropertyOwning 'TypeLevel
type PropertyOwningVL = PropertyOwning 'ValueLevel

