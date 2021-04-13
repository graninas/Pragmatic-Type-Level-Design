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


data AuctionFlowTag a
data LotProcessTag a

type family MkAuctionFlow (a :: *) :: AuctionFlowTag a
type family MkLotProcess  (a :: *) :: LotProcessTag a

data AuctionFlow'     (lotProcess :: LotProcessTag lp)
data LotProcess'      (startActions :: *)

type AuctionFlow lotProcess       = MkAuctionFlow (AuctionFlow' lotProcess)
type LotProcess startActions      = MkLotProcess (LotProcess' startActions)
