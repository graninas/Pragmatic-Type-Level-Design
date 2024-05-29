{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module TypeLevel.ZeplrogOOP.Static.Model.Property where

import CPrelude

import TypeSelector.Granular
import TypeLevel.ZeplrogOOP.Static.Model.Common

import GHC.TypeLits
import qualified Text.Show as T


------ Property -----

-- | Used to make static property hierarchies.
-- Essence arg: own essence
-- Property arg: parent property
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
  -- When materialized, becomes a dict with keys taken from properties
  PropKeyBag :: Essence lvl -> [PropertyOwning lvl] -> PropertyKeyValue lvl
  -- | Separate property
  PropKeyVal :: Essence lvl -> PropertyOwning lvl -> PropertyKeyValue lvl

-- | Static property that must be stat and dyn materialized.
data Property (lvl :: Level) where

  -- | Static properties hierarchy.
  --   Can only be 1 instance of each.
  --   Static props can be referenced by StaticPropertyId
  --     OR by EssenceVL which should be unique to them.
  --   These properties can't be dyn materialized.
  StaticProp
    :: PropertyGroup lvl
    -> Property lvl

  -- | Static prop reference.
  --   When StaticPropRef is dyn materialized,
  --   becomes own dyn prop with a reference to the static prop.
  StaticPropRef
    :: Property lvl
    -> Property lvl

  -- | Compound property.
  -- Each prop in the bag is a mutable reference.
  -- Each prop can be replaced by some other prop in the dyn model.
  PropDict
    :: PropertyGroup lvl
    -> [PropertyKeyValue lvl]
    -> Property lvl

  -- | Abstract property (identical to PropDict).
  --   Provides the shape for the derived properties.
  --   Should not be dynamically materialized.
  AbstractProp
    :: PropertyGroup lvl
    -> [PropertyKeyValue lvl]
    -> Property lvl

  -- | Derived property.
  --    Will take the shape of the parent, with certain props
  --    (of the 1st level) replaced.
  --    After static materialization, becomes PropDict.
  --    No value-type static or dynamic props correspond to it.
  DerivedProp
    :: Essence lvl
    -> Property lvl
    -> [PropertyKeyValue lvl]
    -> Property lvl

  -- -- | Property script.
  -- PropScript
  --   :: PropertyGroup lvl
  --   -> Script lvl
  --   -> Property lvl


------ Short identifiers ----------

type PropertyTL = Property 'TypeLevel
type PropertyVL = Property 'ValueLevel

type PropertyGroupTL = PropertyGroup 'TypeLevel
type PropertyGroupVL = PropertyGroup 'ValueLevel

type PropertyKeyValueTL = PropertyKeyValue 'TypeLevel
type PropertyKeyValueVL = PropertyKeyValue 'ValueLevel

type PropertyOwningTL = PropertyOwning 'TypeLevel
type PropertyOwningVL = PropertyOwning 'ValueLevel

