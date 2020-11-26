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

data AsIntroAuctionFlow = AsIntroAuctionFlow
data AsIntroLotProcess  = AsIntroLotProcess
data AsIntroAction      = AsIntroAction

-- AuctionFlow

instance (Eval AsIntroLotProcess proc [String]) =>
  Eval AsIntroAuctionFlow (AuctionFlow' proc) [String] where
  eval _ _ = do
    strs <- eval AsIntroLotProcess (Proxy :: Proxy proc)
    pure $ "AuctionFlow" : strs

instance (mkAuct ~ MkAuctionFlow auct, Eval AsIntroAuctionFlow auct [String]) =>
  Eval AsIntroAuctionFlow mkAuct [String] where
  eval _ _ = eval AsIntroAuctionFlow (Proxy :: Proxy auct)

-- Lot Process

instance (Eval AsIntroAction acts [String]) =>
  Eval AsIntroLotProcess (LotProcess' acts) [String] where
  eval _ _ = do
    strs <- eval AsIntroAction (Proxy :: Proxy acts)
    pure $ "Lot process" : strs

instance (mkProc ~ MkLotProcess proc, Eval AsIntroLotProcess proc [String]) =>
  Eval AsIntroLotProcess mkProc [String] where
  eval _ _ = eval AsIntroLotProcess (Proxy :: Proxy proc)

-- The Actions mechanism

-- This is how we unwrap a type constructed with a type family.
-- Example:
-- type End = MkAction End'
--      ^ type to unwrap
--            ^ type family
--                     ^ some real data type

instance (mkAct ~ MkAction act, Eval AsIntroAction act [String]) =>
  Eval AsIntroAction mkAct [String] where
  eval _ _ = eval AsIntroAction (Proxy :: Proxy act)

instance Eval AsIntroAction End' [String] where
  eval _ _ = pure ["End' reached."]

instance (Eval AsIntroAction act [String], Eval AsIntroAction acts [String]) =>
  Eval AsIntroAction (Action' act acts) [String] where
  eval _ _ = do
    strs1 <- eval AsIntroAction (Proxy :: Proxy act)
    strs2 <- eval AsIntroAction (Proxy :: Proxy acts)
    pure $ strs1 <> strs2

-- Specific actions

instance Eval AsIntroAction (GetPayloadValue' valName valType lam) [String] where
  eval _ _ = pure ["GetPayloadValue' reached"]
