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

import Auction.Language.Description

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- User space

data USD
data EUR
data GBP

data AllowedCountries' (name :: Symbol) (participants :: [ Country ])

data EFLotPayload' (minBid :: MoneyConstTag a)

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

type AllowedCountries = AllowedCountries'     -- Just a synonym
type EFLotPayload     = EFLotPayload'         -- Just a synonym
