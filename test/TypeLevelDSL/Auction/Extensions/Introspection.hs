{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.Auction.Extensions.Introspection where

import TypeLevelDSL.Auction.Description.Language
import TypeLevelDSL.Auction.Description.Introspection
import TypeLevelDSL.Auction.Extensions.Language
import TypeLevelDSL.Eval

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation

-- Interpreting of the participants list

data AsParticipants = AsParticipants

-- Empty list of participants is not allowed.
-- instance ParticipantInfo p => Eval AsParticipants '[] [String] where
--   eval _ _ = pure []

instance ParticipantInfo p => Eval AsParticipants (p ': '[]) String where
  eval _ _ = pure $ showParticipant (Proxy :: Proxy p)

instance (ParticipantInfo p, Eval AsParticipants (x ': xs) String) =>
  Eval AsParticipants (p ': x ': xs) String where
  eval _ _ = do
    ps <- eval AsParticipants (Proxy :: Proxy (x ': xs))
    let p = showParticipant (Proxy :: Proxy p)
    pure $ p <> ", " <> ps


-- Interpreting of the AllowedCountries censorship

instance (Eval AsParticipants participants String) =>
  Eval AsCensorship (AllowedCountries name participants) [String] where
  eval _ _ = do
    participants <- eval AsParticipants (Proxy :: Proxy participants)
    pure [ "Eligible participants: " <> participants ]


-- Interpreting of the specific currency

instance Eval AsCurrency GBP [String] where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy :: Proxy GBP) ]

instance Eval AsCurrency USD [String] where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy :: Proxy USD) ]

instance Eval AsCurrency EUR [String] where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy :: Proxy EUR) ]

-- Interpreting other extensions

-- Dynamic (runtime) value.
-- N.B., this sample does not check for type safety of the money value.
instance Eval AsMoneyConst (DynVal "202 min bid") String where
  eval _ _ = pure "20000.0"

-- Payload
instance Eval AsMoneyConst minBid String =>
  Eval AsLotPayload (Payload minBid) String where
  eval _ _ = do
    v <- eval AsMoneyConst (Proxy :: Proxy minBid)
    pure $ "Minimum bid: " <> v
