{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}

module Auction.Language.Auction where

import GHC.TypeLits (Symbol)

import Auction.Language.Flow as X


data Country
  = US
  | UK
  | UA
  deriving (Show, Read, Eq, Ord)

data MinBid

newtype ValNameSymb = ValNameS Symbol

-- Interfaces / extension points

data IMoneyConst where
  MoneyConstWrapper :: a -> IMoneyConst

data IAuctionInfo where
  AuctionInfoWrapper :: a -> IAuctionInfo

data ILotPayload where
  LotPayloadWrapper :: a -> ILotPayload

data ICurrency where
  CurrencyWrapper :: a -> ICurrency

data ICensorship where
  CensorshipWrapper :: a -> ICensorship

data IBid where
  BidWrapper :: a -> IBid

data ILot where
  LotWrapper :: a -> ILot

-- Construction

type family MkMoneyConst (a :: *) :: IMoneyConst where
  MkMoneyConst a = MoneyConstWrapper a

type family MkAuctionInfo (a :: *) :: IAuctionInfo where
  MkAuctionInfo a = AuctionInfoWrapper a

type family MkCurrency (a :: *) :: ICurrency where
  MkCurrency a = CurrencyWrapper a

type family MkCensorship (a :: *) :: ICensorship where
  MkCensorship a = CensorshipWrapper a

type family MkLotPayload (a :: *) :: ILotPayload where
  MkLotPayload a = LotPayloadWrapper a

type family MkBid (a :: *) :: IBid where
  MkBid a = BidWrapper a

type family MkLot (a :: *) :: ILot where
  MkLot a = LotWrapper a


-- Implementations

data DynValImpl (name :: ValNameSymb)

data MoneyValImpl (val :: Symbol)

data InfoImpl (name :: Symbol) (holder :: Symbol)

data LotImpl
  (name :: Symbol)
  (description :: Symbol)
  (payload :: ILotPayload)
  (currency :: ICurrency)
  (censorship :: ICensorship)

data NoCensorshipImpl

data AuctionImpl
  (auctionFlow :: IAuctionFlow)
  (auctionInfo :: IAuctionInfo)
  (lots        :: [ILot])         -- TODO: use non-empty list

-- Smart constructors

type MoneyVal (val :: Symbol) = MkMoneyConst (MoneyValImpl val)
type MoneyDynVal (valName :: ValNameSymb) = MkMoneyConst (DynValImpl valName)

type NoCensorship     = MkCensorship NoCensorshipImpl
type Info name holder = MkAuctionInfo (InfoImpl name holder)
type Censorship c     = MkCensorship c
type LotPayload p     = MkLotPayload p
type Currency c       = MkCurrency c
type Lot n d p c cur  = MkLot (LotImpl n d p c cur)
type Auction          = AuctionImpl       -- Just a type synonym

