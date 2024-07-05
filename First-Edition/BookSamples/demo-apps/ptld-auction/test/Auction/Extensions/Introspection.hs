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

import Auction.Language
import Auction.Introspection.Auction
import Auction.Extensions.Language
import TypeLevelDSL.Eval

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


data AsIntroMoneyConst = AsIntroMoneyConst

-- MoneyConst

instance
  ( KnownSymbol val
  ) =>
  Eval AsIntroMoneyConst (MoneyValImpl val) (IO String) where
  eval _ _ = pure $ symbolVal $ Proxy @val

instance
  ( Eval AsIntroMoneyConst m (IO String)
  ) =>
  Eval AsIntroMoneyConst (MoneyConstWrapper m) (IO String) where
  eval _ _ = eval AsIntroMoneyConst $ Proxy @m


-- Interpreting of the participants list

data AsIntroParticipants = AsIntroParticipants

instance
  Eval AsIntroParticipants '[] (IO String) where
  eval _ _ = pure []

instance
  ( ParticipantInfo p
  , Eval AsIntroParticipants xs (IO String)
  ) =>
  Eval AsIntroParticipants (p ': xs) (IO String) where
  eval _ _ = do
    ps <- eval AsIntroParticipants $ Proxy @xs
    let p = showParticipant $ Proxy @p
    pure $ p <> ", " <> ps


-- Interpreting of the AllowedCountries censorship

instance
  ( Eval AsIntroParticipants participants (IO String)
  ) =>
  Eval AsIntroCensorship
    (AllowedCountriesImpl name participants)
    (IO [String]) where
  eval _ _ = do
    participants <- eval AsIntroParticipants $ Proxy @participants
    pure [ "Eligible participants: " <> participants ]


-- Interpreting of the specific currency

instance Eval AsIntroCurrency GBP (IO [String]) where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy @GBP) ]

instance Eval AsIntroCurrency USD (IO [String]) where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy @USD) ]

instance Eval AsIntroCurrency EUR (IO [String]) where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy @EUR) ]

-- Interpreting other extensions

-- Dynamic (runtime) value. For now hardcoded but can be obtained from any source.
-- N.B., this sample does not check for type safety of the money value.
type MinBid202 = 'ValNameS "202 min bid"
instance
  Eval AsIntroMoneyConst (DynValImpl MinBid202) (IO String) where
  eval _ _ = pure "20000"

-- Payload
instance
  ( Eval AsIntroMoneyConst minBid (IO String)
  ) =>
  Eval AsIntroLotPayload (EFLotPayloadImpl minBid) (IO String) where
  eval _ _ = do
    v <- eval AsIntroMoneyConst $ Proxy @minBid
    pure $ "Minimum bid: " <> v
