{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Auction.Extensions.Language where

import Auction.Language

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


-- User space

data IMoneyConst where
  MoneyConstWrapper :: a -> IMoneyConst

type family MkMoneyConst (a :: *) :: IMoneyConst where
  MkMoneyConst a = MoneyConstWrapper a

type MoneyVal (val :: Symbol) = MkMoneyConst (MoneyValImpl val)
type MoneyDynVal (valName :: ValNameSymb) = MkMoneyConst (DynValImpl valName)

data USD
data EUR
data GBP

data AllowedCountriesImpl
  (name :: Symbol)
  (participants :: [ Country ])

data EFLotPayloadImpl
  (minBid :: IMoneyConst)

class CurrencyInfo a where
  showCurrency :: Proxy a -> String

instance CurrencyInfo USD where showCurrency _ = "USD"
instance CurrencyInfo EUR where showCurrency _ = "EUR"
instance CurrencyInfo GBP where showCurrency _ = "GBP"

class ParticipantInfo a where
  showParticipant :: Proxy a -> String

instance ParticipantInfo US where showParticipant _ = "US"
instance ParticipantInfo UK where showParticipant _ = "UK"
instance ParticipantInfo UA where showParticipant _ = "UA"

type AllowedCountries name cs
  = Censorship (AllowedCountriesImpl name cs)

type EFLotPayload b = MkLotPayload (EFLotPayloadImpl b)
