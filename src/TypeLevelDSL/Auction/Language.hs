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

module TypeLevelDSL.Auction.Language where

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
  deriving (Show, Read, Eq, Ord)

-- eDSL

data Auction (auctionInfo :: AuctionInfoTag i) (lots :: LotsTag ls)

data Info (name :: Symbol) (aType :: AuctionType) (holder :: Symbol)

data Lot (name :: Symbol) (descr :: Symbol) (currency :: CurrencyTag a) (censorship :: CensorshipTag c)

-- Extension points:

data AuctionInfoTag a
data LotsTag a
data CurrencyTag a
data CensorshipTag a

-- Construction

type family AuctionInfo (a :: *) :: AuctionInfoTag a

type family Currency (a :: *) :: CurrencyTag a

type family Censorship (a :: *) :: CensorshipTag a

type family Lots (a :: [*]) :: LotsTag a

-- Helpers

data NoCensorship'

type NoCensorship = Censorship NoCensorship'
