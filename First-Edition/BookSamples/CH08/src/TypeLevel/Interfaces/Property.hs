{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

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

data IField
type family MkField (a :: *) :: IField

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
  (ess   :: IEssence)
  (props :: [IProperty])
type KeyBagField ess props = MkField (KeyBagFieldImpl ess props)

data KeyValFieldImpl
  (ess :: IEssence)
  (own :: IPropertyOwning)
type KeyValField ess own = MkField (KeyValFieldImpl ess own)

-- -- Abstract property implementation

data AbstractPropertyImpl
  (pg :: IPropertyGroup)
  (fs :: [IField])
type AbstractProp pg fs = MkAbstractProperty (AbstractPropertyImpl pg fs)

data AbstractDerivedPropImpl
  (ess   :: IEssence)
  (aProp :: IAbstractProperty)
  (fs    :: [IField])
type AbstractDerivedProp ess aProp fs = MkAbstractProperty (AbstractDerivedPropImpl ess aProp fs)

-- -- Property implementation

data DerivedPropImpl
  (ess   :: IEssence)
  (aProp :: IAbstractProperty)
  (fs    :: [IField])
type DerivedProp pg aProp fs = MkProperty (DerivedPropImpl pg aProp fs)
