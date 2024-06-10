{-# LANGUAGE DataKinds #-}

module TypeLevel.ZeplrogOOP.Dynamic.Model.Common where

import CPrelude

import qualified TypeLevel.ZeplrogOOP.Static.Model as SMod

import qualified GHC.Types as GHC

type Essence = String
type EssencePath = [Essence]

type TypeTag = String
type StringifiedValue = String

data Value
  = PairValue Value Value
  | IntValue Int
  | BoolValue Bool
  | StringValue String
  | TagValue SMod.TagPropertyVL Value
  | PathValue [Essence]
  | StaticPropertyRefValue SMod.StaticPropertyId
  deriving (Show, Eq, Ord)
