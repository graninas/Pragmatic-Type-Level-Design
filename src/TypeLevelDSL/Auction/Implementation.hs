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
  , AsImplAuction (..)
  , describeAuction
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified TypeLevelDSL.Auction.Description.Language as L
import qualified TypeLevelDSL.Auction.Flow.Language as L
import qualified TypeLevelDSL.Auction.Language as L
import qualified TypeLevelDSL.Auction.Description.Implementation as Impl
import qualified TypeLevelDSL.Auction.Description.Interpretation as I
-- import TypeLevelDSL.Auction.Flow.Implementation as X
import TypeLevelDSL.Eval

-- Interpretation tags

data AsImplAuction = AsImplAuction

-- Interpreting of the Auction

for_ :: [a] -> (a -> m b) -> m ()
for_ = flip mapM_

instance
  ( Eval AsImplLots lots Impl.Lots
  , Eval I.AsIntroLots lots [String]
  , Eval AsImplAuctionFlow flow ()
  ) =>
  Eval AsImplAuction (Auction' flow info lots) () where
  eval _ _ = do

    putStrLn "Auction is started."

    lots       <- eval AsImplLots (Proxy :: Proxy lots)
    lotsDescrs <- eval I.AsIntroLots (Proxy :: Proxy lots)

    for_ (zip lots lotsDescrs) $ \((lot, lotP), descr) -> do
      mapM_ putStrLn descr
      evalCtx lot AsImplLotProcess lotP

    -- get a min bid for a lot
    -- strs3 <- eval AsImplAuctionFlow (Proxy :: Proxy flow)
    pure ()


runAuction
  :: Eval AsImplAuction auction ()
  => Proxy auction
  -> IO ()
runAuction p = eval AsImplAuction p
