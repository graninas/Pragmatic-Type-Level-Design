{-# LANGUAGE DataKinds #-}

module TypeLevel.ZeplrogOOP.Dynamic.Model.Common where

import CPrelude

import qualified TypeLevel.ZeplrogOOP.Static.Model as SMod


type Essence = String

data Value
  = PairValue Value Value
  | IntValue Int
  | BoolValue Bool
  | StringValue String
  | Path [Essence]
  | StaticPropertyRefValue SMod.StaticPropertyId
  deriving (Show, Eq, Ord)
