{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}

module TypeLevelDSL.DataClassification.Language where

import GHC.TypeLits (Symbol)


-- Closed data

data Country
  = US
  | UK
  | UA
  deriving (Show, Read, Eq, Ord)

-- We don't support other types of auctions in our dsl.
data AuctionType
  = EnglishAuction

-- eDSL

data MoneyVal' (val :: Symbol)

data Auction (auctionInfo :: AuctionInfoTag i) (lots :: LotsTag ls)

data Info' (name :: Symbol) (aType :: AuctionType) (holder :: Symbol)

data Lot (name :: Symbol) (descr :: Symbol) (minBid :: MoneyConstTag m) (currency :: CurrencyTag a) (censorship :: CensorshipTag c)

-- Extension points:

data MoneyConstTag a
data AuctionInfoTag a
data LotsTag a
data CurrencyTag a
data CensorshipTag a

-- Construction

type family MoneyConst (a :: *) :: MoneyConstTag a

type family AuctionInfo (a :: *) :: AuctionInfoTag a

type family Currency (a :: *) :: CurrencyTag a

type family Censorship (a :: *) :: CensorshipTag a

type family Lots (a :: [*]) :: LotsTag a

-- Helpers

data NoCensorship'

type NoCensorship = Censorship NoCensorship'

type Info name aType holder = AuctionInfo (Info' name aType holder)

type MoneyVal (val :: Symbol) = MoneyConst (MoneyVal' val)
