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

module TypeLevelDSL.Auction.Description.Implementation where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified TypeLevelDSL.Auction.Description.Language as L
import TypeLevelDSL.Eval

data Lot = Lot
type Lots = Lots


-- Interpretation tags

data AsImplInfo       = AsImplInfo
data AsImplLots       = AsImplLots
data AsImplLot        = AsImplLot
data AsImplCensorship = AsImplCensorship
data AsImplCurrency   = AsImplCurrency
data AsImplMoneyConst = AsImplMoneyConst
data AsImplLotPayload = AsImplLotPayload
data AsImplBid        = AsImplBid
data AsImplMinBid     = AsImplMinBid


-- Interpreting of the auction info (auctionInfo :: AuctionInfoTag)

instance (ai ~ MkAuctionInfo i, Eval AsImplInfo i [String]) =>
  Eval AsImplInfo ai [String] where
  eval _ _ = eval AsImplInfo (Proxy :: Proxy i)

instance (KnownSymbol name, KnownSymbol holder) =>
  Eval AsImplInfo (Info' name holder) [String] where
  eval _ _ = do
    pure $ ( "Name: " <> symbolVal (Proxy :: Proxy name) )
         : ( "Holder: " <> symbolVal (Proxy :: Proxy holder) )
         : []

-- Interpreting of the list of lots (lots :: LotsTag a)

-- No instance for an empty list. Empty lists are prohibited.
-- instance Eval AsImplLots '[] [String] where
--   eval _ _ = pure []

-- N.B., item is interpreted AsImplLot
instance Eval AsImplLot p Lot =>
  Eval AsImplLots (p ': '[]) Lots where
  eval _ _ = eval AsImplLot (Proxy :: Proxy p)

-- N.B., item is interpreted AsImplLot
instance
  ( Eval AsImplLot p Lot
  , Eval AsImplLots (x ': ps) Lots
  ) =>
  Eval AsImplLots (p ': x ': ps) Lots where
  eval _ _ = do
    lot  <- eval AsImplLot (Proxy :: Proxy p)
    lots <- eval AsImplLots (Proxy :: Proxy (x ': ps))
    pure $ lot : lots

-- Entry point into the Lots type level list & DSL item
instance (b ~ L.MkLots a, Eval AsImplLots a Lots) =>
  Eval AsImplLots b Lots where
  eval _ _ = eval AsImplLots (Proxy :: Proxy a)


-- Interpreting of a Lot

instance
  ( Eval AsImplCurrency currency [String]
  , Eval AsImplCensorship censorship [String]
  , Eval AsImplLotPayload payload String
  , KnownSymbol name
  , KnownSymbol descr
  ) =>
  Eval AsImplLot (L.Lot' name descr payload currency censorship) Lot where
  eval _ _ = do

    pure $ Lot 
    -- payload    <- eval AsImplLotPayload (Proxy :: Proxy payload)
    -- censorship <- eval AsImplCensorship (Proxy :: Proxy censorship)
    -- currency   <- eval AsImplCurrency (Proxy :: Proxy currency)
    -- pure $ ( "Lot: " <> symbolVal (Proxy :: Proxy name) )
    --      : ( "Description: " <> symbolVal (Proxy :: Proxy descr) )
    --      :   payload
    --      : ( currency <> censorship )


-- Interpreting of the Currency extension

instance (b ~ MkCurrency a, Eval AsImplCurrency a [String]) =>
  Eval AsImplCurrency b [String] where
  eval _ _ = eval AsImplCurrency (Proxy :: Proxy a)


-- Interpreting of the Censorship extension

instance (b ~ MkCensorship a, Eval AsImplCensorship a [String]) =>
  Eval AsImplCensorship b [String] where
  eval _ _ = eval AsImplCensorship (Proxy :: Proxy a)


-- Interpretating of the NoCensorship

instance Eval AsImplCensorship NoCensorship' [String] where
  eval _ _ = pure []


-- Interpreting a MoneyConst value

instance (b ~ MkMoneyConst a, Eval AsImplMoneyConst a String) =>
  Eval AsImplMoneyConst b String where
  eval _ _ = eval AsImplMoneyConst (Proxy :: Proxy a)

instance KnownSymbol val =>
 Eval AsImplMoneyConst (MoneyVal' val) String where
  eval _ _ = pure $ symbolVal (Proxy :: Proxy val)

-- Interpreting a LotPayload value

instance (b ~ MkLotPayload a, Eval AsImplLotPayload a String) =>
  Eval AsImplLotPayload b String where
  eval _ _ = eval AsImplLotPayload (Proxy :: Proxy a)
