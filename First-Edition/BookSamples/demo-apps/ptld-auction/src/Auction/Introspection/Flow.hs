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

module Auction.Introspection.Flow where

import Auction.Introspection.Action
import Auction.Language.Flow
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation

-- Interpreting of the participants list

data AsIntroAuctionFlow = AsIntroAuctionFlow
data AsIntroLotProcess  = AsIntroLotProcess

-- AuctionFlow

instance (Eval AsIntroLotProcess proc (IO [String])) =>
  Eval AsIntroAuctionFlow (AuctionFlowImpl proc) (IO [String]) where
  eval _ _ = do
    strs <- eval AsIntroLotProcess (Proxy :: Proxy proc)
    pure $ "AuctionFlow" : strs

instance
  ( mkAuct ~ MkAuctionFlow auct
  , Eval AsIntroAuctionFlow auct (IO [String])
  ) =>
  Eval AsIntroAuctionFlow mkAuct (IO [String]) where
  eval _ _ = eval AsIntroAuctionFlow (Proxy :: Proxy auct)

-- Lot Process

instance (Eval AsIntroAction acts (IO [String])) =>
  Eval AsIntroLotProcess (LotProcessImpl acts) (IO [String]) where
  eval _ _ = do
    strs <- eval AsIntroAction (Proxy :: Proxy acts)
    pure $ "Lot process" : strs

instance
  ( mkProc ~ MkLotProcess proc
  , Eval AsIntroLotProcess proc (IO [String])
  ) =>
  Eval AsIntroLotProcess mkProc (IO [String]) where
  eval _ _ = eval AsIntroLotProcess (Proxy :: Proxy proc)
