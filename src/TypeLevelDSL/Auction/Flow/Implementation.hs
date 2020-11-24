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

module TypeLevelDSL.Auction.Flow.Implementation where

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

instance (EvalCtx ctx AsLotProcess proc [String]) =>
  EvalCtx ctx AsAuctionFlow (AuctionFlow' proc) [String] where
  evalCtx ctx _ _ = do
    strs <- evalCtx ctx AsLotProcess (Proxy :: Proxy proc)
    pure $ "AuctionFlow" : strs

instance (mkAuct ~ MkAuctionFlow auct, EvalCtx ctx AsAuctionFlow auct [String]) =>
  EvalCtx ctx AsAuctionFlow mkAuct [String] where
  evalCtx ctx _ _ = evalCtx ctx AsAuctionFlow (Proxy :: Proxy auct)

-- Lot Process

instance (EvalCtx ctx AsAction acts [String]) =>
  EvalCtx ctx AsLotProcess (LotProcess' acts) [String] where
  evalCtx ctx _ _ = do
    strs <- evalCtx ctx AsAction (Proxy :: Proxy acts)
    pure $ "Lot process" : strs

instance (mkProc ~ MkLotProcess proc, EvalCtx ctx AsLotProcess proc [String]) =>
  EvalCtx ctx AsLotProcess mkProc [String] where
  evalCtx ctx _ _ = evalCtx ctx AsLotProcess (Proxy :: Proxy proc)

-- The Actions mechanism

-- This is how we unwrap a type constructed with a type family.
-- Example:
-- type End = MkAction End'
--      ^ type to unwrap
--            ^ type family
--                     ^ some real data type

instance (mkAct ~ MkAction act, EvalCtx ctx AsAction act [String]) =>
  EvalCtx ctx AsAction mkAct [String] where
  evalCtx ctx _ _ = evalCtx ctx AsAction (Proxy :: Proxy act)

instance EvalCtx ctx AsAction End' [String] where
  evalCtx ctx _ _ = pure ["End' reached."]

instance (EvalCtx ctx AsAction act [String], EvalCtx ctx AsAction acts [String]) =>
  EvalCtx ctx AsAction (Action' act acts) [String] where
  evalCtx ctx _ _ = do
    strs1 <- evalCtx ctx AsAction (Proxy :: Proxy act)
    strs2 <- evalCtx ctx AsAction (Proxy :: Proxy acts)
    pure $ strs1 <> strs2

-- Specific actions

instance EvalCtx ctx AsAction (GetPayloadValue' valName valType lam) [String] where
  evalCtx ctx _ _ = pure ["GetPayloadValue' reached"]
