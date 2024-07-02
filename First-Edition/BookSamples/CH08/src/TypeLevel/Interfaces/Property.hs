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
data IUnknown a
type family MkUnknown (a :: *) :: IUnknown a

data IPropertyGroup a
type family MkPropertyGroup (a :: *) :: IPropertyGroup a

data IAbstractProperty a
type family MkAbstractProperty (a :: *) :: IAbstractProperty a

data IProperty a
type family MkProperty (a :: *) :: IProperty a

data IProperties a
type family MkProperties (a :: [IProperty b]) :: IProperties a

data IField a
type family MkField (a :: *) :: IField a

data IFields a
type family MkFields (a :: [IField b]) :: IFields a

data IPropertyOwning a
type family MkPropertyOwning (a :: *) :: IPropertyOwning a


-- Implementations

-- -- IUnknown placeholder implementation

data UnknownImpl
type Unknown = MkUnknown UnknownImpl
data DummyImpl
type Dummy = MkUnknown DummyImpl

-- -- Group implementation

data GroupImpl (ess :: IEssence a)
type Group ess = MkPropertyGroup (GroupImpl ess)

data GroupRootImpl (ess :: IEssence a) (prop :: IProperty b)
type GroupRoot ess prop = MkPropertyGroup (GroupRootImpl ess prop)

-- -- Property owning implementation

data OwnValImpl (valDef :: IUnknown a)
type OwnVal (unkn :: IUnknown a) = MkPropertyOwning (OwnValImpl unkn)

data OwnPropImpl (prop :: IProperty a)
type OwnProp (prop :: IProperty a) = MkPropertyOwning (OwnPropImpl prop)

-- -- Property field implementation

data KeyBagFieldImpl
  (ess :: IEssence a)
  (props :: IProperties b)
type KeyBagField ess props = MkField (KeyBagFieldImpl ess props)

data KeyValFieldImpl
  (ess :: IEssence a)
  (own :: IPropertyOwning b)
type KeyValField ess own = MkField (KeyValFieldImpl ess own)

-- -- Abstract property implementation

data AbstractPropertyImpl
  (pg :: IPropertyGroup a)
  (fs :: IFields b)
type AbstractProp pg fs = MkAbstractProperty (AbstractPropertyImpl pg fs)

data AbstractDerivedPropImpl
  (ess :: IEssence a)
  (aProp :: IAbstractProperty b)
  (fs :: IFields c)
type AbstractDerivedProp ess aProp fs = MkAbstractProperty (AbstractDerivedPropImpl ess aProp fs)

-- -- Property implementation
data DerivedPropImpl
  (ess :: IEssence a)
  (aProp :: IAbstractProperty b)
  (fs :: IFields c)
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

