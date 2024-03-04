{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- type family Lots (a :: [*]) :: LotsTag a
--                                        ^
{-# LANGUAGE PolyKinds                #-}

-- data D (name :: Symbol) (ps :: [Country])
--                 ^              ^
{-# LANGUAGE KindSignatures           #-}

module Auction.Language.Description where

import GHC.TypeLits (Symbol)

-- Closed data

data Country
  = US
  | UK
  | UA
  deriving (Show, Read, Eq, Ord)

-- This is not Symbol because it's known at the design time.
-- Symbol should be used when the name of a value is not related to the domain directly
-- (like, service or temporary variables etc)
data MinBid

-- eDSL

newtype ValNameSymb = ValNameS Symbol

data DynVal' (name :: ValNameSymb)

data MoneyVal' (val :: Symbol)

data Info' (name :: Symbol) (holder :: Symbol)

data Lot' (name :: Symbol)
          (description :: Symbol)
          (payload :: LotPayloadTag p)
          (currency :: CurrencyTag a)
          (censorship :: CensorshipTag c)

data NoCensorship'    -- This can be pattern matched esier than something like ()

-- Extension points:

data MoneyConstTag a
data AuctionInfoTag a
data LotsTag a
data LotPayloadTag a
data CurrencyTag a
data CensorshipTag a
data BidTag a

-- Construction

type family MkMoneyConst  (a :: *)   :: MoneyConstTag a
type family MkAuctionInfo (a :: *)   :: AuctionInfoTag a
type family MkCurrency    (a :: *)   :: CurrencyTag a
type family MkCensorship  (a :: *)   :: CensorshipTag a
type family MkLots        (a :: [*]) :: LotsTag a
type family MkLotPayload  (a :: *)   :: LotPayloadTag a
type family MkBid         (a :: *)   :: BidTag a

-- Helpers

type NoCensorship                 = MkCensorship NoCensorship'
type Info name holder             = MkAuctionInfo (Info' name holder)
type MoneyVal (val :: Symbol)     = MkMoneyConst (MoneyVal' val)
type MoneyDynVal (valName :: ValNameSymb) = MkMoneyConst (DynVal' valName)
type Censorship c                 = MkCensorship c
type LotPayload p                 = MkLotPayload p
type Currency c                   = MkCurrency c
type Lots ls                      = MkLots ls
type Lot                          = Lot'                       -- Just a synonym
