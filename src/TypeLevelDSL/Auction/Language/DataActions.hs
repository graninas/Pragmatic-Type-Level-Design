{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.Auction.Language.DataActions where

import TypeLevelDSL.Auction.Language.System
import TypeLevelDSL.Auction.Language.Action

import GHC.TypeLits (Symbol, Nat)


-- Should valName be a Symbol?
data GetPayloadValue' (valName :: *) (valType :: *) (lam :: LambdaTag lamBody)
data ReadRef' (ref :: RefTagTag a) (lam :: LambdaTag lamBody)
data WriteRef' (ref :: RefTagTag a)


-- Helpers

type GetPayloadValue n t lam = MkAction (GetPayloadValue' n t lam)
type ReadRef ref lam = MkAction (ReadRef' ref lam)
type WriteRef ref = MkLambda (WriteRef' ref)
