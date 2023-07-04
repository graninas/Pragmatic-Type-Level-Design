{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Auction.Language.DataActions where

import TypeLevelDSL.Language.Action

import GHC.TypeLits (Symbol, Nat)

data LotName
data LotDescr

-- Should valName be a Symbol?
data GetPayloadValue' (valTag :: *) (valType :: *) (lam :: LambdaTag lamBody)
data GetLotName' (lam :: LambdaTag lamBody)      -- custom methods
data GetLotDescr' (lam :: LambdaTag lamBody)     -- custom methods


-- Helpers

type GetPayloadValue tag typ lam = MkAction (GetPayloadValue' tag typ lam)
type GetLotName lam              = MkAction (GetLotName' lam)     -- Custom methods
type GetLotDescr lam             = MkAction (GetLotDescr' lam)    -- Custom methods
type GetLotName2 lam             = MkAction (GetPayloadValue' LotName  String lam)
type GetLotDescr2 lam            = MkAction (GetPayloadValue' LotDescr String lam)
