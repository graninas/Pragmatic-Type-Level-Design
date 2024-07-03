{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeLevel.Interfaces.Property where

import CPrelude

import TypeLevel.Interfaces.Common

import GHC.TypeLits


-- | Interface for a non-implemented functionality
data IUnknown
type family MkUnknown (a :: *) :: IUnknown

data IPropertyGroup
type family MkPropertyGroup (a :: *) :: IPropertyGroup

data IAbstractProperty
type family MkAbstractProperty (a :: *) :: IAbstractProperty

data IProperty
type family MkProperty (a :: *) :: IProperty

data IProperties
type family MkProperties (a :: [IProperty]) :: IProperties

data IField
type family MkField (a :: *) :: IField

data IFields
type family MkFields (a :: [IField]) :: IFields

data IPropertyOwning
type family MkPropertyOwning (a :: *) :: IPropertyOwning


-- Implementations

-- -- IUnknown placeholder implementation

data UnknownImpl
type Unknown = MkUnknown UnknownImpl
data DummyImpl
type Dummy = MkUnknown DummyImpl

-- -- Group implementation

data GroupImpl (ess :: IEssence)
type Group ess = MkPropertyGroup (GroupImpl ess)

data GroupRootImpl (ess :: IEssence) (prop :: IProperty)
type GroupRoot ess prop = MkPropertyGroup (GroupRootImpl ess prop)

-- -- Property owning implementation

data OwnValImpl (valDef :: IUnknown)
type OwnVal (unkn :: IUnknown) = MkPropertyOwning (OwnValImpl unkn)

data OwnPropImpl (prop :: IProperty)
type OwnProp (prop :: IProperty) = MkPropertyOwning (OwnPropImpl prop)

-- -- Property field implementation

data KeyBagFieldImpl
  (ess :: IEssence)
  (props :: IProperties)
type KeyBagField ess props = MkField (KeyBagFieldImpl ess props)

data KeyValFieldImpl
  (ess :: IEssence)
  (own :: IPropertyOwning)
type KeyValField ess own = MkField (KeyValFieldImpl ess own)

-- -- Abstract property implementation

data AbstractPropertyImpl
  (pg :: IPropertyGroup)
  (fs :: IFields)
type AbstractProp pg fs = MkAbstractProperty (AbstractPropertyImpl pg fs)

data AbstractDerivedPropImpl
  (ess :: IEssence)
  (aProp :: IAbstractProperty)
  (fs :: IFields)
type AbstractDerivedProp ess aProp fs = MkAbstractProperty (AbstractDerivedPropImpl ess aProp fs)

-- -- Property implementation
data DerivedPropImpl
  (ess :: IEssence)
  (aProp :: IAbstractProperty)
  (fs :: IFields)
type DerivedProp pg aProp fs = MkProperty (DerivedPropImpl pg aProp fs)

type Fields fs = MkFields fs
type Properties ps = MkProperties ps

-- data PropertyScript (lvl :: Level) where
--   PropScript
--     :: Essence lvl
--     -> CustomScript lvl
--     -> PropertyScript lvl



-- -- | Static property that must be stat and dyn materialized.
-- data Property (lvl :: Level) where
--   -- | Tag property reference.
--   TagPropRef
--     :: TagProperty lvl
--     -> Property lvl

--   -- | Derived property from any abstract property.
--   --    Will take the shape of the parent.
--   --    After static materialization, becomes PropDict.
--   --    DeriviedProp can't be value level.
--   DerivedProp
--     :: EssenceTL
--     -> AbstractProperty
--     -> [PropertyKeyValueTL]
--     -> [PropertyScriptTL]
--     -> PropertyTL

--   -- | Compound property for static value-level and dynamic
--   -- value-level representation.
--   -- Can't be type-level; use AbstractProp and DerivedProp instead.
--   PropDict
--     :: PropertyGroupVL
--     -> [PropertyKeyValueVL]
--     -> [PropertyScriptVL]
--     -> PropertyVL


-- ------ Short identifiers ----------

-- type PropertyGroupTL = PropertyGroup 'TypeLevel
-- type PropertyGroupVL = PropertyGroup 'ValueLevel

-- type PropertyScriptTL = PropertyScript 'TypeLevel
-- type PropertyScriptVL = PropertyScript 'ValueLevel

-- type PropertyTL = Property 'TypeLevel
-- type PropertyVL = Property 'ValueLevel

-- type PropertyKeyValueTL = PropertyKeyValue 'TypeLevel
-- type PropertyKeyValueVL = PropertyKeyValue 'ValueLevel

-- type PropertyOwningTL = PropertyOwning 'TypeLevel
-- type PropertyOwningVL = PropertyOwning 'ValueLevel

