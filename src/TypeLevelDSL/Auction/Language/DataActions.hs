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

import TypeLevelDSL.Auction.Language.Action

import GHC.TypeLits (Symbol, Nat)


-- Should valName be a Symbol?
data GetPayloadValue' (valName :: *) (valType :: *) (lam :: LambdaTag lamBody)
data ReadRef' (refName :: Symbol) (t :: *) (lam :: LambdaTag lamBody)
data WriteRef' (refName :: Symbol) (t :: *)


-- Helpers

type GetPayloadValue n t lam = MkAction (GetPayloadValue' n t lam)
type ReadRef n t lam = MkAction (ReadRef' n t lam)
type WriteRef n t = MkAction (WriteRef' n t)
