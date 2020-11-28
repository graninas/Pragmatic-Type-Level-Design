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

module TypeLevelDSL.Auction.Implementation.Description where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified TypeLevelDSL.Auction.Types as T
import qualified TypeLevelDSL.Auction.Description.Language as L
import qualified TypeLevelDSL.Auction.Implementation.Types as Impl
import TypeLevelDSL.Eval

-- Interpretation tags

data AsImplInfo       = AsImplInfo
data AsImplLots       = AsImplLots
data AsImplLot        = AsImplLot
data AsImplLotPayload = AsImplLotPayload
data AsImplMoneyConst = AsImplMoneyConst

-- data AsImplBid        = AsImplBid
-- data AsImplMinBid     = AsImplMinBid

-- Interpreting of the list of lots (lots :: LotsTag a)

instance Eval AsImplLot p Impl.Lot =>
  Eval AsImplLots (p ': '[]) Impl.Lots where
  eval _ _ = eval AsImplLot (Proxy :: Proxy p)

instance
  ( Eval AsImplLot p Impl.Lot
  , Eval AsImplLots (x ': ps) Impl.Lots
  ) =>
  Eval AsImplLots (p ': x ': ps) Impl.Lots where
  eval _ _ = do
    lot  <- eval AsImplLot (Proxy :: Proxy p)
    lots <- eval AsImplLots (Proxy :: Proxy (x ': ps))
    pure $ lot : lots

instance (b ~ L.MkLots a, Eval AsImplLots a Impl.Lots) =>
  Eval AsImplLots b Impl.Lots where
  eval _ _ = eval AsImplLots (Proxy :: Proxy a)

-- Interpreting of a Lot

instance
  ( Eval AsImplLotPayload payload Impl.Payload
  , KnownSymbol name
  , KnownSymbol descr
  ) =>
  Eval AsImplLot (L.Lot' name descr payload currency censorship) Impl.Lot where
  eval _ _ = do
    payload <- eval AsImplLotPayload (Proxy :: Proxy payload)
    pure $ Lot (symbolVal (Proxy :: Proxy name)) (symbolVal (Proxy :: Proxy descr)) (Impl.startBid payload)

-- Interpreting a MoneyConst value

instance (b ~ MkMoneyConst a, Eval AsImplMoneyConst a T.Money) =>
  Eval AsImplMoneyConst b T.Money where
  eval _ _ = eval AsImplMoneyConst (Proxy :: Proxy a)

instance KnownSymbol val =>
 Eval AsImplMoneyConst (L.MoneyVal' val) T.Money where
  eval _ _ = pure $ read $ symbolVal (Proxy :: Proxy val)     -- unsafe

-- Interpreting a LotPayload value

instance (b ~ MkLotPayload a, Eval AsImplLotPayload a Impl.Payload) =>
  Eval AsImplLotPayload b Impl.Payload where
  eval _ _ = eval AsImplLotPayload (Proxy :: Proxy a)
