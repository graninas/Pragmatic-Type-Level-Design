{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module ZeplrogOOP.Static.Model.Property where

import CPrelude

import ZeplrogOOP.Static.Model.Common

import GHC.TypeLits


-- | Interface for a non-implemented functionality
data IUnknown where
  UnknownWrapper :: a -> IUnknown
type family MkUnknown (a :: *) :: IUnknown where
  MkUnknown a = 'UnknownWrapper a

data IPropertyGroup where
  PropertyGroupWrapper :: a -> IPropertyGroup
type family MkPropertyGroup (a :: *) :: IPropertyGroup where
  MkPropertyGroup a = 'PropertyGroupWrapper a

data IAbstractProperty where
  AbstractPropertyWrapper :: a -> IAbstractProperty
type family MkAbstractProperty (a :: *) :: IAbstractProperty where
  MkAbstractProperty a = 'AbstractPropertyWrapper a

data IProperty where
  PropertyWrapper :: a -> IProperty
type family MkProperty (a :: *) :: IProperty where
  MkProperty a = 'PropertyWrapper a

data IField where
  FieldWrapper :: a -> IField
type family MkField (a :: *) :: IField where
  MkField a = 'FieldWrapper a

data IPropertyOwning where
  PropertyOwningWrapper :: a -> IPropertyOwning

type family MkPropertyOwning (a :: *) :: IPropertyOwning where
  MkPropertyOwning a = 'PropertyOwningWrapper a

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
