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

import TypeLevelDSL.Auction.Flow.Language
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation

-- Interpreting of the participants list

data AsImplAuctionFlow = AsImplAuctionFlow
data AsImplLotProcess  = AsImplLotProcess
data AsImplAction      = AsImplAction

-- AuctionFlow

instance (Eval AsImplLotProcess proc [String]) =>
  Eval AsImplAuctionFlow (AuctionFlow' proc) [String] where
  eval _ _ = do
    strs <- eval AsImplLotProcess (Proxy :: Proxy proc)
    pure $ "AuctionFlow" : strs

instance (mkAuct ~ MkAuctionFlow auct, Eval AsImplAuctionFlow auct [String]) =>
  Eval AsImplAuctionFlow mkAuct [String] where
  eval _ _ = eval AsImplAuctionFlow (Proxy :: Proxy auct)

-- Lot Process

instance (Eval AsImplAction acts [String]) =>
  Eval AsImplLotProcess (LotProcess' acts) [String] where
  eval _ _ = do
    strs <- eval AsImplAction (Proxy :: Proxy acts)
    pure $ "Lot process" : strs

instance (mkProc ~ MkLotProcess proc, Eval AsImplLotProcess proc [String]) =>
  Eval AsImplLotProcess mkProc [String] where
  eval _ _ = eval AsImplLotProcess (Proxy :: Proxy proc)

-- The Actions mechanism

-- This is how we unwrap a type constructed with a type family.
-- Example:
-- type End = MkAction End'
--      ^ type to unwrap
--            ^ type family
--                     ^ some real data type

instance (mkAct ~ MkAction act, Eval AsImplAction act [String]) =>
  Eval AsImplAction mkAct [String] where
  eval _ _ = eval AsImplAction (Proxy :: Proxy act)

instance Eval AsImplAction End' [String] where
  eval _ _ = pure ["End' reached."]

instance (Eval AsImplAction act [String], Eval AsImplAction acts [String]) =>
  Eval AsImplAction (Action' act acts) [String] where
  eval _ _ = do
    strs1 <- eval AsImplAction (Proxy :: Proxy act)
    strs2 <- eval AsImplAction (Proxy :: Proxy acts)
    pure $ strs1 <> strs2

-- Specific actions

instance Eval AsImplAction (GetPayloadValue' valName valType lam) [String] where
  eval _ _ = pure ["GetPayloadValue' reached"]
