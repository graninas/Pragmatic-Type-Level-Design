{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- type family Lots (a :: [*]) :: ILots
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
          (currency :: ICurrency)
          (censorship :: CensorshipTag c)

data NoCensorship'    -- This can be pattern matched esier than something like ()

-- Extension points:

data IMoneyConst
data IAuctionInfo
data ILots
data ILotPayload
data ICurrency
data ICensorship
data IBidTag

-- Construction

type family MkMoneyConst  (a :: *)   :: IMoneyConst
type family MkAuctionInfo (a :: *)   :: IAuctionInfo
type family MkCurrency    (a :: *)   :: ICurrency
type family MkCensorship  (a :: *)   :: ICensorship
type family MkLots        (a :: [*]) :: ILots
type family MkLotPayload  (a :: *)   :: ILotPayload
type family MkBid         (a :: *)   :: IBidTag

-- Helpers

type MoneyVal (val :: Symbol) = MkMoneyConst (MoneyVal' val)
type MoneyDynVal (valName :: ValNameSymb) = MkMoneyConst (DynVal' valName)

type Info name holder = MkAuctionInfo (Info' name holder)
type NoCensorship     = MkCensorship NoCensorship'
type Censorship c     = MkCensorship c
type LotPayload p     = MkLotPayload p
type Currency c       = MkCurrency c
type Lots ls          = MkLots ls
type Lot              = Lot'                       -- Just a synonym
