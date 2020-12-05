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

instance (EvalCtx ctx AsImplLotProcess proc LotProcessFlow) =>
  EvalCtx ctx AsImplAuctionFlow (L.AuctionFlow' proc) AuctionFlow where
  evalCtx ctx _ _ = do
    lotProc <- evalCtx ctx AsImplLotProcess (Proxy :: Proxy proc)
    pure $ \(lot, descr) -> do
      putStrLn "New lot!"
      putStrLn descr
      lotProc lot

instance (mkAuct ~ L.MkAuctionFlow auct, EvalCtx ctx AsImplAuctionFlow auct AuctionFlow) =>
  EvalCtx ctx AsImplAuctionFlow mkAuct AuctionFlow where
  evalCtx ctx _ _ = evalCtx ctx AsImplAuctionFlow (Proxy :: Proxy auct)

-- Lot Process

instance (EvalCtx ctx Impl.AsImplAction acts [String]) =>
  EvalCtx ctx AsImplLotProcess (L.LotProcess' acts) LotProcessFlow where
  evalCtx ctx _ _ = do
    lotProc <- evalCtx ctx Impl.AsImplAction (Proxy :: Proxy acts)
    pure $ \lot -> pure ()

instance
  ( mkProc ~ L.MkLotProcess proc
  , EvalCtx ctx AsImplLotProcess proc LotProcessFlow
  ) =>
  EvalCtx ctx AsImplLotProcess mkProc LotProcessFlow where
  evalCtx ctx _ _ = evalCtx ctx AsImplLotProcess (Proxy :: Proxy proc)
