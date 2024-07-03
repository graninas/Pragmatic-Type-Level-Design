{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Auction.Language.Flow where

import GHC.TypeLits (Symbol, Nat)


data IAuctionFlow
data ILotProcess

type family MkAuctionFlow (a :: *) :: IAuctionFlow
type family MkLotProcess  (a :: *) :: ILotProcess

data AuctionFlow' (lotProcess :: ILotProcess)
data LotProcess'  (startActions :: *)

type AuctionFlow lotProcess  = MkAuctionFlow (AuctionFlow' lotProcess)
type LotProcess startActions = MkLotProcess (LotProcess' startActions)
