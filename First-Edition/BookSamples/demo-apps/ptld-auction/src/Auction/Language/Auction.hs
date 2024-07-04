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
  , AuctionImpl
  ) where

import GHC.TypeLits (Symbol)

import Auction.Language.Description as X
import Auction.Language.Flow as X



data AuctionImpl
  (auctionFlow :: IAuctionFlow a)
  (auctionInfo :: IAuctionInfo b)
  (lots        :: ILots c)


type Auction = AuctionImpl       -- Just a type synonym
