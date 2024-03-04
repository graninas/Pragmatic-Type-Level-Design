{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Auction.Extensions.Introspection where

import Auction.Language.Description
import Auction.Introspection.Description
import Auction.Extensions.Language
import TypeLevelDSL.Eval

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation

-- Interpreting of the participants list

data AsIntroParticipants = AsIntroParticipants

-- Empty list of participants is not allowed.
-- instance ParticipantInfo p => Eval AsIntroParticipants '[] [String] where
--   eval _ _ = pure []

instance ParticipantInfo p =>
  Eval AsIntroParticipants (p ': '[]) (IO String) where
  eval _ _ = pure $ showParticipant (Proxy :: Proxy p)

instance
  ( ParticipantInfo p
  , Eval AsIntroParticipants (x ': xs) (IO String)
  ) =>
  Eval AsIntroParticipants (p ': x ': xs) (IO String) where
  eval _ _ = do
    ps <- eval AsIntroParticipants (Proxy :: Proxy (x ': xs))
    let p = showParticipant (Proxy :: Proxy p)
    pure $ p <> ", " <> ps


-- Interpreting of the AllowedCountries censorship

instance (Eval AsIntroParticipants participants (IO String)) =>
  Eval AsIntroCensorship (AllowedCountries' name participants) (IO [String]) where
  eval _ _ = do
    participants <- eval AsIntroParticipants (Proxy :: Proxy participants)
    pure [ "Eligible participants: " <> participants ]


-- Interpreting of the specific currency

instance Eval AsIntroCurrency GBP (IO [String]) where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy :: Proxy GBP) ]

instance Eval AsIntroCurrency USD (IO [String]) where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy :: Proxy USD) ]

instance Eval AsIntroCurrency EUR (IO [String]) where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy :: Proxy EUR) ]

-- Interpreting other extensions

-- Dynamic (runtime) value. For now hardcoded but can be obtained from any source.
-- N.B., this sample does not check for type safety of the money value.
type MinBid202 = 'ValNameS "202 min bid"
instance Eval AsIntroMoneyConst (DynVal' MinBid202) (IO String) where
  eval _ _ = pure "20000"

-- Payload
instance Eval AsIntroMoneyConst minBid (IO String) =>
  Eval AsIntroLotPayload (EFLotPayload' minBid) (IO String) where
  eval _ _ = do
    v <- eval AsIntroMoneyConst (Proxy :: Proxy minBid)
    pure $ "Minimum bid: " <> v
