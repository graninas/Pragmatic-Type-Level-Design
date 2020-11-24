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

module TypeLevelDSL.Auction.Flow.Introspection where

import TypeLevelDSL.Auction.Flow.Language
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation

-- Interpreting of the participants list

data AsAuctionFlow = AsAuctionFlow
data AsLotProcess  = AsLotProcess
data AsAction      = AsAction

-- AuctionFlow

instance (Eval AsLotProcess proc [String]) =>
  Eval AsAuctionFlow (AuctionFlow' proc) [String] where
  eval _ _ = do
    strs <- eval AsLotProcess (Proxy :: Proxy proc)
    pure $ "AuctionFlow" : strs

instance (mkAuct ~ MkAuctionFlow auct, Eval AsAuctionFlow auct [String]) =>
  Eval AsAuctionFlow mkAuct [String] where
  eval _ _ = eval AsAuctionFlow (Proxy :: Proxy auct)

-- Lot Process

instance (Eval AsAction acts [String]) =>
  Eval AsLotProcess (LotProcess' acts) [String] where
  eval _ _ = do
    strs <- eval AsAction (Proxy :: Proxy acts)
    pure $ "Lot process" : strs

instance (mkProc ~ MkLotProcess proc, Eval AsLotProcess proc [String]) =>
  Eval AsLotProcess mkProc [String] where
  eval _ _ = eval AsLotProcess (Proxy :: Proxy proc)

-- The Actions mechanism

-- This is how we unwrap a type constructed with a type family.
-- Example:
-- type End = MkAction End'
--      ^ type to unwrap
--            ^ type family
--                     ^ some real data type

instance (mkAct ~ MkAction act, Eval AsAction act [String]) =>
  Eval AsAction mkAct [String] where
  eval _ _ = eval AsAction (Proxy :: Proxy act)

instance Eval AsAction End' [String] where
  eval _ _ = pure ["End' reached."]

instance (Eval AsAction act [String], Eval AsAction acts [String]) =>
  Eval AsAction (Action' act acts) [String] where
  eval _ _ = do
    strs1 <- eval AsAction (Proxy :: Proxy act)
    strs2 <- eval AsAction (Proxy :: Proxy acts)
    pure $ strs1 <> strs2

-- Specific actions

instance Eval AsAction (GetPayloadValue' valName valType lam) [String] where
  eval _ _ = pure ["GetPayloadValue' reached"]
