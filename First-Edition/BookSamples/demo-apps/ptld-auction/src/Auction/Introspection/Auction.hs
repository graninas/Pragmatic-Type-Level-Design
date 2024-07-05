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
{-# LANGUAGE AllowAmbiguousTypes      #-}

module Auction.Introspection.Auction where

import Auction.Language
import Auction.Introspection.DataActions

import TypeLevelDSL.Eval
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)


data AsIntroLotPayload = AsIntroLotPayload
data AsIntroInfo = AsIntroInfo
data AsIntroCensorship = AsIntroCensorship
data AsIntroCurrency = AsIntroCurrency
data AsIntroLot = AsIntroLot
data AsIntroAuction = AsIntroAuction
data AsIntroAuctionFlow = AsIntroAuctionFlow

-- Wrappers (extension points)

-- -- AuctionInfoWrapper

instance
  ( Eval AsIntroInfo i (IO [String])
  ) =>
  Eval AsIntroInfo (AuctionInfoWrapper i) (IO [String]) where
  eval _ _ = eval AsIntroInfo $ Proxy @i

-- -- Lot payload wrapper

instance
  ( Eval AsIntroLotPayload p (IO String)
  ) =>
  Eval AsIntroLotPayload (LotPayloadWrapper p) (IO String) where
  eval _ _ = eval AsIntroLotPayload $ Proxy @p

-- -- Currency wrapper

instance
  ( Eval AsIntroCurrency p (IO [String])
  ) =>
  Eval AsIntroCurrency (CurrencyWrapper p) (IO [String]) where
  eval _ _ = eval AsIntroCurrency $ Proxy @p

-- -- Censorship wrapper

instance
  ( Eval AsIntroCensorship p (IO [String])
  ) =>
  Eval AsIntroCensorship (CensorshipWrapper p) (IO [String]) where
  eval _ _ = eval AsIntroCensorship $ Proxy @p

-- -- Auction flow wrapper

instance
  ( Eval AsIntroAuctionFlow p (IO [String])
  ) =>
  Eval AsIntroAuctionFlow (AuctionFlowWrapper p) (IO [String]) where
  eval _ _ = eval AsIntroAuctionFlow $ Proxy @p

-- -- Lot wrapper

instance
  ( Eval AsIntroLot p (IO [String])
  ) =>
  Eval AsIntroLot (LotWrapper p) (IO [String]) where
  eval _ _ = eval AsIntroLot $ Proxy @p

-- Instances

-- Info

instance
  ( KnownSymbol name
  , KnownSymbol holder
  ) =>
  Eval AsIntroInfo (InfoImpl name holder) (IO [String]) where
  eval _ _ = do
    pure [ "Name: "   <> (symbolVal $ Proxy @name)
         , "Holder: " <> (symbolVal $ Proxy @holder)
         ]

-- -- Lot list

instance
  Eval AsIntroLot '[] (IO [String]) where
  eval _ _ = pure []

instance
  ( Eval AsIntroLot p (IO [String])
  , Eval AsIntroLot ps (IO [String])
  ) =>
  Eval AsIntroLot (p ': ps) (IO [String]) where
  eval _ _ = do
    strs1 <- eval AsIntroLot $ Proxy @p
    strs2 <- eval AsIntroLot $ Proxy @ps
    pure $ strs1 <> strs2

-- -- Lot

instance
  ( Eval AsIntroCurrency currency (IO [String])
  , Eval AsIntroCensorship censorship (IO [String])
  , Eval AsIntroLotPayload payload (IO String)
  , KnownSymbol name
  , KnownSymbol descr
  ) =>
  Eval AsIntroLot
    (LotImpl
      name
      descr
      payload
      currency
      censorship)
    (IO [String]) where
  eval _ _ = do
    payload    <- eval AsIntroLotPayload $ Proxy @payload
    censorship <- eval AsIntroCensorship $ Proxy @censorship
    currency   <- eval AsIntroCurrency   $ Proxy @currency

    pure $
      [ "Lot: "         <> (symbolVal $ Proxy @name)
      , "Description: " <> (symbolVal $ Proxy @descr)
      , payload
      ] <> currency
        <> censorship

-- -- Censorship

instance Eval AsIntroCensorship NoCensorshipImpl (IO [String]) where
  eval _ _ = pure []

-- -- AuctionFlow

instance
  ( Eval AsIntroAction acts (IO [String])
  ) =>
  Eval AsIntroAuctionFlow
    (AuctionFlowImpl acts)
    (IO [String]) where
  eval _ _ = do
    strs <- eval AsIntroAction $ Proxy @acts
    pure $ "AuctionFlow" : strs

-- -- Auction

instance
  ( Eval AsIntroInfo info (IO [String])
  , Eval AsIntroLot lots (IO [String])
  , Eval AsIntroAuctionFlow flow (IO [String])
  ) =>
  Eval AsIntroAuction
    (AuctionImpl flow info lots)
    (IO [String]) where
  eval _ _ = do

    -- start the flow
    strs1 <- eval AsIntroInfo $ Proxy @info
    strs2 <- eval AsIntroLot $ Proxy @lots

    -- get a min bid for a lot
    strs3 <- eval AsIntroAuctionFlow $ Proxy @flow
    pure $ "==> Auction! <==" : (strs1 <> strs2 <> strs3)


describeAuction
  :: Eval AsIntroAuction auction (IO a)
  => Proxy auction
  -> IO a
describeAuction p = eval AsIntroAuction p
