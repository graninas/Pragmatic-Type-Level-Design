{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.AuctionSpec where

import TypeLevelDSL.Auction.Language
import TypeLevelDSL.Auction.Implementation
import TypeLevelDSL.Eval

import Test.Hspec

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- User space

data USD
data EUR
data GBP

data AllowedCountries (name :: Symbol) (participants :: [ Country ])

data Payload (minBid :: MoneyConstTag a)

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
  Eval AsCensorship (AllowedCountries name participants) Ret where
  eval _ _ = do
    participants <- eval AsParticipants (Proxy :: Proxy participants)
    pure [ "Eligible participants: " <> participants ]


-- Interpreting of the specific currency

instance Eval AsCurrency GBP Ret where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy :: Proxy GBP) ]

instance Eval AsCurrency USD Ret where
  eval _ _ = pure [ "Currency: " <> showCurrency (Proxy :: Proxy USD) ]

instance Eval AsCurrency EUR Ret where
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

-- Test sample

type UKOnly  = Censorship (AllowedCountries "UK only" '[UK])
type UKAndUS = Censorship (AllowedCountries "UK & US" '[UK, US])

type PayloadLot1 = LotPayload (Payload (MoneyVal "1000.0"))
type PayloadLot2 = LotPayload (Payload (MoneyDynVal "202 min bid"))
type PayloadLot3 = LotPayload (Payload (MoneyVal "40000.0"))

type WorldArtsAuction = Auction
  (Info "World arts" EnglishAuction "UK Bank")
  (Lots '[ Lot "101" "Dali artwork"      PayloadLot1 (Currency GBP) UKOnly
         , Lot "202" "Chinese vase"      PayloadLot2 (Currency USD) UKAndUS
         , Lot "303" "Ancient mechanism" PayloadLot3 (Currency USD) NoCensorship
         ]
  )

-- English Auction Flow

-- Greeting
-- Iterations
-- Bid
-- Lot
-- Notification
-- Final condition
-- Round

-- Lenses?
-- data LotPayloadGetter
-- data MinBidGetter
-- data Get l r

data LotProcessTag a
data LotRoundTag a
data WinnerTag a
data StartActionTag a
data EndOnTag a
data RoundDefTag a
data NoActionRoundsTag a


type family Winner (a :: *) :: WinnerTag a
type family EndOn (a :: *) :: EndOnTag a
type family StartAction (a :: *) :: StartActionTag a
type family LotProcess (a :: *) :: LotProcessTag a
type family LotRound (a :: [*]) :: LotRoundTag a


type StartFrom bidQuery winner = StartAction (StartAction' bidQuery winner)


data NoWinner'
type NoWinner = Winner NoWinner'

data NoActionRounds (cnt :: Nat)

data StartAction' (bidQuery :: BidTag a) (winner :: WinnerTag b)
data EndCondition' (endOn :: EndOnTag a)

data LotProcess' (startFrom :: StartFromTag a) (endOn :: EndOnTag b) (lotRound :: LotRoundTag c)

data AuctionFlow (lotProcess :: LotProcessTag a)

type EnglishAuctionFlow = AuctionFlow
  (LotProcess
      (StartFrom (Bid "minBid") NoWinner)
      (EndOn (NoActionRounds 3))
      (LotRound '[])
  )




runner :: IO [String]
runner = eval AsAuction (Proxy :: Proxy WorldArtsAuction)


spec :: Spec
spec =
  describe "Type level Servant-like eDSL Auction" $ do
    it "Run WorldArtsAuction script" $ do
      strs <- runner
      -- putStrLn $ intercalate "\n" strs
      strs `shouldBe`
        [ "==> Auction! <=="
        , "Name: World arts"
        , "Holder: UK Bank"
        , "Type: EnglishAuction"
        , "Lot: 101"
        , "Description: Dali artwork"
        , "Minimum bid: 1000.0"
        , "Currency: GBP"
        , "Eligible participants: UK"
        , "Lot: 202"
        , "Description: Chinese vase"
        , "Minimum bid: 20000.0"
        , "Currency: USD"
        , "Eligible participants: UK, US"
        , "Lot: 303"
        , "Description: Ancient mechanism"
        , "Minimum bid: 40000.0"
        , "Currency: USD"
        ]
