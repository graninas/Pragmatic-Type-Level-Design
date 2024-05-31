{-# LANGUAGE DataKinds #-}

module TypeLevel.ZeplrogOOP.Dynamic.Model.Property where

import CPrelude

import qualified TypeLevel.ZeplrogOOP.Static.Model as SMod
import TypeLevel.ZeplrogOOP.Dynamic.Model.Common

import qualified Data.Map.Strict as Map


-- | Dynamic property Id
newtype PropertyId = PropertyId Int
  deriving (Show, Eq, Ord)

-- | Property owning
data PropertyOwning
  = OwnVal (TVar Value)
  -- ^ Mutable dynamic value
  | OwnDict (TVar (Map.Map Essence Property))
  -- ^ Multiple child props
  | OwnProp Property
  -- ^ Single child prop
  | SharedProp PropertyRef
  -- ^ Reference to an independent prop

-- | Reference to another independent property
data PropertyRef
  = DynamicPropRef PropertyId
  | StaticPropRef SMod.StaticPropertyId

-- | Dynamic property
data Property
  = TagPropRef SMod.TagPropertyVL
  | Prop
    { pPropertyId       :: PropertyId
      -- ^ Unique Id for a property instance
    , pOwner            :: Maybe PropertyId
      -- ^ Independent property that owns this prop exclusively
    , pStaticPropertyId :: SMod.StaticPropertyId
      -- ^ Source property for this one
    , pPropertyBagVar   :: TVar (Map.Map Essence PropertyOwning)
      -- ^ Child properties
    }
