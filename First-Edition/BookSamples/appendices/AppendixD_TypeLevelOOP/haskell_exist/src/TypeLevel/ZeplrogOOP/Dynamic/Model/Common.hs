{-# LANGUAGE DataKinds #-}

module TypeLevel.ZeplrogOOP.Dynamic.Model.Common where

import CPrelude

import qualified TypeLevel.ZeplrogOOP.Static.Model as SMod


type Essence = String
type EssencePath = [Essence]

data Value
  = PairValue Value Value
  | IntValue Int
  | BoolValue Bool
  | StringValue String
  | TagValue SMod.TagPropertyVL Value
  | Path [Essence]
  | StaticPropertyRefValue SMod.StaticPropertyId
  deriving (Show, Eq, Ord)
