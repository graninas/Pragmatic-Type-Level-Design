module Minefield.Extensions.Nouns.Landmine.Def where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits


-- | Landmine definition.
--  Landmines may have arbitrary power.
data LandmineDef
  (icon :: Symbol)
  (objectType :: Symbol)
  (power :: Nat)
    -- ^ Detonation power from 1 to 3.
    --   1 == only explodes itself
    --   n == triggers neighbor bombs to explode in the nth radius
type Landmine i p = MkObjectTemplate (LandmineDef i "landmine" p)
