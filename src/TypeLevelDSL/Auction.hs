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

module TypeLevelDSL.Auction where

import           Data.Proxy (Proxy(..))
import           GHC.TypeLits (Symbol)


-- Closed data

data Country
  = US
  | UK
  | UA

-- We don't support other types of auctions in our dsl.
data AuctionType
  = EnglishAuction


-- eDSL


data Auction (auctionInfo :: AuctionInfoTag) (lots :: LotsTag ls)

data Info (name :: Symbol) (aType :: AuctionType) (holder :: Symbol)

data Lot (name :: Symbol) (descr :: Symbol) (currency :: CurrencyTag a) (censorship :: CensorshipTag c)

data LotsTag a

data AuctionInfoTag

data CurrencyTag a
data CensorshipTag a


-- Construction of extensions

-- stock space

type family AuctionInfo (a :: *) :: AuctionInfoTag

type family Currency (c :: *) :: CurrencyTag c

type family Censorship (a :: *) :: CensorshipTag a

type family Lots (a :: [*]) :: LotsTag a





-- user space

data USD

data AllowedCountries (name :: Symbol) (participants :: [ Country ])








-- Test sample

type UKOnly = Censorship (AllowedCountries "UK only" '[UK])
type Info1 = AuctionInfo (Info "UK Art" EnglishAuction "UK Bank")
type Lot1 = Lot "101" "Dali artwork" (Currency USD) UKOnly
type Auction1 = Auction Info1 (Lots '[])
