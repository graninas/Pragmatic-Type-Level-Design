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

import Auction.Language.DataActions

import GHC.TypeLits (Symbol, Nat)


data IAuctionFlow where
  AuctionFlowWrapper :: a -> IAuctionFlow

type family MkAuctionFlow (a :: *) :: IAuctionFlow where
  MkAuctionFlow a = AuctionFlowWrapper a

data AuctionFlowImpl (acts :: [IAction])

type AuctionFlow acts
  = MkAuctionFlow (AuctionFlowImpl acts)




