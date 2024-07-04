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

data DynValImpl (name :: ValNameSymb)

data MoneyValImpl (val :: Symbol)

data InfoImpl (name :: Symbol) (holder :: Symbol)

data LotImpl
  (name :: Symbol)
  (description :: Symbol)
  (payload :: ILotPayload p)
  (currency :: ICurrency a)
  (censorship :: ICensorship c)

data NoCensorshipImpl    -- This can be pattern matched esier than something like ()

-- Extension points:

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

-- Helpers

type NoCensorship                 = MkCensorship NoCensorshipImpl
type Info name holder             = MkAuctionInfo (InfoImpl name holder)
type MoneyVal (val :: Symbol)     = MkMoneyConst (MoneyValImpl val)
type MoneyDynVal (valName :: ValNameSymb) = MkMoneyConst (DynValImpl valName)
type Censorship c                 = MkCensorship c
type LotPayload p                 = MkLotPayload p
type Currency c                   = MkCurrency c
type Lots ls                      = MkLots ls
type Lot                          = LotImpl                       -- Just a synonym
