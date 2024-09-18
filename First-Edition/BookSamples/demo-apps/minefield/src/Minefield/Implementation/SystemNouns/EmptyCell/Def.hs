module Minefield.Implementation.SystemNouns.EmptyCell.Def where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits


-- | Empty cell definition.
data EmptyCellDef
  (icon :: Symbol)
  (objectType :: Symbol)
type EmptyCell i = MkObjectTemplate (EmptyCellDef i "empty-cell")
