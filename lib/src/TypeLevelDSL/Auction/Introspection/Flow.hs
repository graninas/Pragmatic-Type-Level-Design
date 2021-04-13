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

module TypeLevelDSL.Auction.Introspection.Flow where

import TypeLevelDSL.Auction.Introspection.Action
import TypeLevelDSL.Auction.Language.Flow
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation

-- Interpreting of the participants list

data AsIntroAuctionFlow = AsIntroAuctionFlow
data AsIntroLotProcess  = AsIntroLotProcess

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
