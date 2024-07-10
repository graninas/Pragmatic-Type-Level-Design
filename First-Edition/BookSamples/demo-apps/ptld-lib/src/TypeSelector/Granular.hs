{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}

module TypeSelector.Granular where

import CPrelude
import GHC.TypeLits

-- | Granular Type Selector is a design pattern, variation
--   of HKD to enhance type-level programming.

data Level = TypeLevel | ValueLevel
  deriving (Show, Eq, Ord)

type family StringType (lvl :: Level) :: a where
  StringType 'TypeLevel  = Symbol
  StringType 'ValueLevel = String     -- TODO: use Text

type family IntegerType (lvl :: Level) where
  IntegerType 'TypeLevel  = Nat
  IntegerType 'ValueLevel = Int
