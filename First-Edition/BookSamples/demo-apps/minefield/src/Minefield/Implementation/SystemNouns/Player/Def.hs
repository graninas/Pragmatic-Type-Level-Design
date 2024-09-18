module Minefield.Implementation.SystemNouns.Player.Def where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits


-- | Player definition.
data PlayerDef
  (icon :: Symbol)
  (objectType :: Symbol)
type Player i = MkObjectTemplate (PlayerDef i "player")
