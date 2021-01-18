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

data LotName
data LotDescr

-- Should valName be a Symbol?
data GetPayloadValue' (valTag :: *) (valType :: *) (lam :: LambdaTag lamBody)
data GetLots (lam :: LambdaTag lamBody)
data GetLotName' (lam :: LambdaTag lamBody)      -- custom methods
data GetLotDescr' (lam :: LambdaTag lamBody)     -- custom methods
data ReadRef' (refName :: Symbol) (t :: *) (lam :: LambdaTag lamBody)
data WriteRef' (refName :: Symbol) (t :: *)
data ForEach (lam :: LambdaTag lamBody)

-- Helpers

type GetPayloadValue tag typ lam = MkAction (GetPayloadValue' tag typ lam)
type GetLotName lam              = MkAction (GetLotName' lam)     -- Custom methods
type GetLotDescr lam             = MkAction (GetLotDescr' lam)    -- Custom methods
type GetLotName2 lam             = MkAction (GetPayloadValue' LotName  String lam)
type GetLotDescr2 lam            = MkAction (GetPayloadValue' LotDescr String lam)
type ReadRef n t lam             = MkAction (ReadRef' n t lam)
type WriteRef n t                = MkLambda (WriteRef' n t)
