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
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation

type AuctionFlow     = (Impl.Lot, String) -> IO ()
type LotProcessFlow  = Impl.Lot -> IO ()

-- Interpreting of the participants list

data AsImplAuctionFlow = AsImplAuctionFlow
data AsImplLotProcess  = AsImplLotProcess
data AsImplAction      = AsImplAction

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

instance (Eval AsImplAction acts [String]) =>
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

-- The Actions mechanism

-- This is how we unwrap a type constructed with a type family.
-- Example:
-- type End = MkAction End'
--      ^ type to unwrap
--            ^ type family
--                     ^ some real data type

instance
  ( mkAct ~ L.MkAction act
  , Eval AsImplAction act [String]
  ) =>
  Eval AsImplAction mkAct [String] where
  eval _ _ = eval AsImplAction (Proxy :: Proxy act)

instance Eval AsImplAction L.End' [String] where
  eval _ _ = pure ["End' reached."]

instance
  ( Eval AsImplAction act [String]
  , Eval AsImplAction acts [String]
  ) =>
  Eval AsImplAction (L.Action' act acts) [String] where
  eval _ _ = do
    strs1 <- eval AsImplAction (Proxy :: Proxy act)
    strs2 <- eval AsImplAction (Proxy :: Proxy acts)
    pure $ strs1 <> strs2

-- Specific actions

instance Eval AsImplAction (L.GetPayloadValue' valName valType lam) [String] where
  eval _ _ = pure ["GetPayloadValue' reached"]
