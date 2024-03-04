{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}

module Auction.Language.Auction
  ( module X
  , Auction
  , Auction'
  ) where

import GHC.TypeLits (Symbol)

import Auction.Language.Description as X
import Auction.Language.Flow as X



data Auction'
  (auctionFlow :: AuctionFlowTag f)
  (auctionInfo :: AuctionInfoTag i)
  (lots        :: LotsTag ls)


type Auction = Auction'       -- Just a type synonym
