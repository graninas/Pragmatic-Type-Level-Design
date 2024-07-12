{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE GADTs                    #-}

module Minefield.Extensions.Nouns.Landmine where

import CPrelude

import Minefield.Core.Language
import Minefield.Core.System
import Minefield.Core.Eval

import GHC.TypeLits


-- | Landmine with variable power.
data LandmineImpl
  (icon :: Symbol)
  (power :: Nat)
    -- ^ Detonation power from 1 to 3.
    --   1 == only explodes itself
    --   n == triggers neighbor bombs to explode in the nth radius
type Landmine i p = MkObject (LandmineImpl i p)



instance
  ( KnownSymbol i
  ) =>
  Eval GetIcon (LandmineImpl i p) Char where
  eval _ _ = head $ symbolVal $ Proxy @i
