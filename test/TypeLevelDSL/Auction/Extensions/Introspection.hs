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

import TypeLevelDSL.Auction.Language.Description
import TypeLevelDSL.Auction.Introspection.Description
import TypeLevelDSL.Auction.Extensions.Language
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
  Eval AsIntroParticipants (p ': '[]) String where
  eval _ _ = pure $ showParticipant (Proxy :: Proxy p)

instance (ParticipantInfo p, Eval AsIntroParticipants (x ': xs) String) =>
  Eval AsIntroParticipants (p ': x ': xs) String where
  eval _ _ = do
    ps <- eval AsIntroParticipants (Proxy :: Proxy (x ': xs))
    let p = showParticipant (Proxy :: Proxy p)
    pure $ p <> ", " <> ps


-- Interpreting of the AllowedCountries censorship

instance (Eval AsIntroParticipants participants String) =>
  Eval AsIntroCensorship (AllowedCountries' name participants) [String] where
  eval _ _ = do
    participants <- eval AsIntroParticipants (Proxy :: Proxy participants)
    pure [ "Eligible participants: " <> participants ]


-- Interpreting of the specific currency

instance Eval AsIntroCurrency GBP [String] where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy :: Proxy GBP) ]

instance Eval AsIntroCurrency USD [String] where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy :: Proxy USD) ]

instance Eval AsIntroCurrency EUR [String] where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy :: Proxy EUR) ]

-- Interpreting other extensions

-- Dynamic (runtime) value. For now hardcoded but can be obtained from any source.
-- N.B., this sample does not check for type safety of the money value.
instance Eval AsIntroMoneyConst (DynVal' "202 min bid") String where
  eval _ _ = pure "20000"

-- Payload
instance Eval AsIntroMoneyConst minBid String =>
  Eval AsIntroLotPayload (EFLotPayload' minBid) String where
  eval _ _ = do
    v <- eval AsIntroMoneyConst (Proxy :: Proxy minBid)
    pure $ "Minimum bid: " <> v
