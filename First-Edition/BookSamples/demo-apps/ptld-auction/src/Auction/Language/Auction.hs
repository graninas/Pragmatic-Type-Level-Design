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

data IMoneyConst a
data IAuctionInfo a
data ILots a
data ILotPayload a
data ICurrency a
data ICensorship a
data IBid a

-- Construction

type family MkMoneyConst  (a :: *)   :: IMoneyConst a
type family MkAuctionInfo (a :: *)   :: IAuctionInfo a
type family MkCurrency    (a :: *)   :: ICurrency a
type family MkCensorship  (a :: *)   :: ICensorship a
type family MkLots        (a :: [*]) :: ILots a
type family MkLotPayload  (a :: *)   :: ILotPayload a
type family MkBid         (a :: *)   :: IBid a

-- Implementations

data DynValImpl (name :: ValNameSymb)

data MoneyValImpl (val :: Symbol)

data InfoImpl (name :: Symbol) (holder :: Symbol)

data LotImpl
  (name :: Symbol)
  (description :: Symbol)
  (payload :: ILotPayload p)
  (currency :: ICurrency a)
  (censorship :: ICensorship c)

data NoCensorshipImpl

data AuctionImpl
  (auctionFlow :: IAuctionFlow a)
  (auctionInfo :: IAuctionInfo b)
  (lots        :: ILots c)

-- Smart constructors

type MoneyVal (val :: Symbol) = MkMoneyConst (MoneyValImpl val)
type MoneyDynVal (valName :: ValNameSymb) = MkMoneyConst (DynValImpl valName)

type NoCensorship     = MkCensorship NoCensorshipImpl
type Info name holder = MkAuctionInfo (InfoImpl name holder)
type Censorship c     = MkCensorship c
type LotPayload p     = MkLotPayload p
type Currency c       = MkCurrency c
type Lots ls          = MkLots ls
type Lot              = LotImpl     -- Just a synonym
type Auction          = AuctionImpl -- Just a type synonym
