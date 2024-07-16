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

import Minefield.Core.Eval
import Minefield.Core.Types
import Minefield.Core.Interface
import Minefield.Core.Object

import Minefield.Extensions.Materialization

import GHC.TypeLits


-- | Landmine with variable power.
data LandmineImpl
  (icon :: Symbol)
  (objectType :: Symbol)
  (power :: Nat)
    -- ^ Detonation power from 1 to 3.
    --   1 == only explodes itself
    --   n == triggers neighbor bombs to explode in the nth radius
type Landmine i p = MkObject (LandmineImpl i "landmine" p)

-- Implementation

getLandmineIcon p = ['A'..'Z'] !! p

instance
  ( KnownNat p
  ) =>
  Eval () GetIcon (LandmineImpl i ot p) Icon where
  eval () _ _ = pure
    $ getLandmineIcon
    $ fromIntegral
    $ natVal
    $ Proxy @p

instance
  ( KnownNat p
  , KnownSymbol ot
  ) =>
  Eval () GetObjectInfo (LandmineImpl i ot p) ObjectInfo where
  eval () _ _ = do
    let oType = symbolVal $ Proxy @ot
    let icon =  getLandmineIcon
                  $ fromIntegral
                  $ natVal
                  $ Proxy @p
    pure $ ObjectInfo icon (-1, -1) oType True []

instance
  ( KnownSymbol ot
  ) =>
  Eval () GetObjectType (LandmineImpl i ot p) ObjectType where
  eval () _ _ = pure $ symbolVal $ Proxy @ot

-- Object

data LandmineObject = LandmineObject
  { loObjectInfoRef :: IORef ObjectInfo
  , loPowerRef      :: IORef Int
  }
