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
data GetPayloadValue' (valTag :: *) (valType :: *) (lam :: LambdaTag lamBody)
data ReadRef' (refName :: Symbol) (t :: *) (lam :: LambdaTag lamBody)
data WriteRef' (refName :: Symbol) (t :: *)


-- Helpers

type GetPayloadValue tag typ lam = MkAction (GetPayloadValue' tag typ lam)
type ReadRef n t lam = MkAction (ReadRef' n t lam)


type WriteRef n t = MkLambda (WriteRef' n t)
