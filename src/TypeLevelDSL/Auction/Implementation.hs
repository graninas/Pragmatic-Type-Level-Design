{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}

module TypeLevelDSL.Auction.Implementation
  ( module X
  , AuctionState (..)
  , AsAuction (..)
  , runAuction
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import TypeLevelDSL.Auction.Description.Language
import TypeLevelDSL.Auction.Flow.Language
import TypeLevelDSL.Auction.Language
import TypeLevelDSL.Auction.Description.Implementation as X
import TypeLevelDSL.Auction.Flow.Implementation as X
import TypeLevelDSL.Eval
import TypeLevelDSL.HasValue



-- TODO: try to make it separated from the implementation
-- by a massive using of the Has pattern
-- and additional mechanisms to switch the state.
data AuctionState
  = AuctionStart
  | AuctionState
  { minBidVal :: Float
  }

-- Interpretation tags

data AsAuction = AsAuction

-- Interpreting of the Auction


instance
  ( Eval AsInfo info [String]
  , Eval AsLots lots [String]
  , EvalCtx AuctionState AsAuctionFlow flow [String]) =>
  EvalCtx AsAuction AuctionState (Auction' flow info lots) [String] where
  evalCtx ctx _ _ = do

    -- start the flow
    strs1 <- eval AsInfo (Proxy :: Proxy info)
    strs2 <- eval AsLots (Proxy :: Proxy lots)

    -- get a min bid for a lot
    strs3 <- evalCtx (AuctionState 1000.0) AsAuctionFlow (Proxy :: Proxy flow)
    pure $ "==> Auction! <==" : (strs1 <> strs2 <> strs3)




runAuction
  :: EvalCtx AsAuction AuctionState auction a
  => Proxy auction -> IO a
runAuction p = evalCtx AsAuction AuctionStart p


instance HasValue AuctionState MinBid Float where
getVal (AuctionState mb) = mb
getVal _ = error "Value is not supported: MinBid"
