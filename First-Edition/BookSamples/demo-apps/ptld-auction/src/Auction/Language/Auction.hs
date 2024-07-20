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

import Auction.Language.DataActions
import Auction.Language.Flow


data Country
  = US
  | UK
  | UA
  deriving (Show, Read, Eq, Ord)

newtype ValNameSymb = ValNameS Symbol

-- Interfaces / extension points

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

data AuctionImpl
  (auctionFlow :: IAuctionFlow)
  (auctionInfo :: IAuctionInfo)
  (lots        :: [ILot])         -- TODO: use non-empty list

-- Smart constructors

type Info name holder = MkAuctionInfo (InfoImpl name holder)
type Censorship c     = MkCensorship c
type Currency c       = MkCurrency c
type Lot n d p c cur  = MkLot (LotImpl n d p c cur)
type LotPayload x     = MkLotPayload x
type Auction          = AuctionImpl       -- Just a type synonym

data NoCensorshipImpl
data NoLotPayloadImpl
type NoCensorship = MkCensorship NoCensorshipImpl
type NoLotPayload = MkLotPayload NoLotPayloadImpl


-- Specials

type LotNameTag  = MkTag "lot name" Symbol String
type LotDescrTag = MkTag "lot descr" Symbol String

type GetLotName
  (lam :: ILambda Symbol outT)
  = GetPayloadValue LotNameTag lam

type GetLotDescr
  (lam :: ILambda Symbol outT)
  = GetPayloadValue LotDescrTag lam




-- Test scenarios


type EnglishAuctionLotAction1 =
  '[ GetLotName (ConcatL "New lot: " PrintF)
   , GetLotDescr PrintF
   ]

type EnglishAuctionLotAction2 =
  '[ GetLotName
        (ConcatL "New lot: "
          (Both (WriteRef Symbol "LotName result") PrintF))

   , GetLotDescr (ConcatL "Lot description: "
        (Both (WriteRef Symbol "LotDescr result") PrintF))
   ]

type EnglishAuctionFlow = AuctionFlow EnglishAuctionLotAction1

type TestFlow = AuctionFlow
  '[ ReadRef Int "curRound" (ShowF Int PrintF)
   , ReadRef Int "curCost" (ShowF Int PrintF)
   ]


type Actions =
  '[ ReadRef Int "val1" (WriteRef Int "val2")
   , ReadRef Int "val2" (WriteRef Int "val1")
   ]

type PrintingNameAction =
  GetLine ((ConcatL "Hello, " (ConcatR PrintF "!")))

type Greetings =
  '[ PrintLine "What is your name?"
   , PrintingNameAction
   ]
