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


data IAuctionFlow a
data ILotProcess a

type family MkAuctionFlow (a :: *) :: IAuctionFlow a
type family MkLotProcess  (a :: *) :: ILotProcess a

data AuctionFlowImpl (lotProcess :: ILotProcess a)
data LotProcessImpl  (startActions :: *)

type AuctionFlow lotProcess
  = MkAuctionFlow (AuctionFlowImpl lotProcess)
type LotProcess startActions
  = MkLotProcess (LotProcessImpl startActions)
