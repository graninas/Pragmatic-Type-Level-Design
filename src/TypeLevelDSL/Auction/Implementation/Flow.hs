{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}

module TypeLevelDSL.Auction.Implementation.Flow where

import qualified TypeLevelDSL.Auction.Types as T
import qualified TypeLevelDSL.Auction.Language as L
import qualified TypeLevelDSL.Auction.Implementation.Types as Impl
import qualified TypeLevelDSL.Auction.Implementation.Action as Impl
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation

type AuctionFlow     = (Impl.Lot, String) -> IO ()
type LotProcessFlow  = Impl.Lot -> IO ()

-- Interpreting of the participants list

data AsImplAuctionFlow = AsImplAuctionFlow
data AsImplLotProcess  = AsImplLotProcess

-- AuctionFlow

instance (Eval AsImplLotProcess proc LotProcessFlow) =>
  Eval AsImplAuctionFlow (L.AuctionFlow' proc) AuctionFlow where
  eval _ _ = do
    lotProc <- eval AsImplLotProcess (Proxy :: Proxy proc)
    pure $ \(lot, descr) -> do
      putStrLn "New lot!"
      putStrLn descr
      lotProc lot

instance (mkAuct ~ L.MkAuctionFlow auct, Eval AsImplAuctionFlow auct AuctionFlow) =>
  Eval AsImplAuctionFlow mkAuct AuctionFlow where
  eval _ _ = eval AsImplAuctionFlow (Proxy :: Proxy auct)

-- Lot Process

instance (Eval Impl.AsImplAction acts [String]) =>
  Eval AsImplLotProcess (L.LotProcess' acts) LotProcessFlow where
  eval _ _ = do
    -- lotProc <- eval AsImplAction (Proxy :: Proxy acts)
    -- pure $ "Lot process" : strs
    pure $ \lot -> pure ()

instance
  ( mkProc ~ L.MkLotProcess proc
  , Eval AsImplLotProcess proc LotProcessFlow
  ) =>
  Eval AsImplLotProcess mkProc LotProcessFlow where
  eval _ _ = eval AsImplLotProcess (Proxy :: Proxy proc)
