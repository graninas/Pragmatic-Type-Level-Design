{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.Auction.Language.Flow where

import GHC.TypeLits (Symbol, Nat)

import TypeLevelDSL.Auction.Language.Action

data AuctionFlowTag a
data LotProcessTag a

type family MkAuctionFlow (a :: *) :: AuctionFlowTag a
type family MkLotProcess  (a :: *) :: LotProcessTag a

data AuctionFlow'     (flowAct :: ActionTag lp)
data LotProcess'      (startActions :: *)

type AuctionFlow flowAct       = MkAuctionFlow (AuctionFlow' flowAct)
type LotProcess startActions   = MkLambda (LotProcess' startActions)
