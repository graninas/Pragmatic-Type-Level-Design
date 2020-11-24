{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.Auction.AuctionSpec where

import TypeLevelDSL.Auction.Language
import TypeLevelDSL.Auction.Implementation
import TypeLevelDSL.Auction.Exts
import TypeLevelDSL.Auction.ExtsImpl
import TypeLevelDSL.Eval
import TypeLevelDSL.HasValue

import Test.Hspec

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


-- Test sample

type UKOnly  = Censorship (AllowedCountries "UK only" '[UK])
type UKAndUS = Censorship (AllowedCountries "UK & US" '[UK, US])

type PayloadLot1 = LotPayload (Payload (MoneyVal "1000.0"))
type PayloadLot2 = LotPayload (Payload (MoneyDynVal "202 min bid"))
type PayloadLot3 = LotPayload (Payload (MoneyVal "40000.0"))

-- Auction algorithm

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

type EnglishAuctionFlow = AuctionFlow
  ( LotProcess
      ( Action (GetPayloadValue MinBid Float Print)
        ( Action (GetPayloadValue MinBid Float Drop)
          End
        )
      )
  )

-- Auction

type WorldArtsInfo = Info "World arts" "UK Bank"
type WorldArtsLots = Lots
  '[ Lot "101" "Dali artwork"      PayloadLot1 (Currency GBP) UKOnly
  , Lot "202" "Chinese vase"      PayloadLot2 (Currency USD) UKAndUS
  , Lot "303" "Ancient mechanism" PayloadLot3 (Currency USD) NoCensorship
  ]

type WorldArtsAuction = Auction
  EnglishAuctionFlow
  WorldArtsInfo
  WorldArtsLots

runner :: IO [String]
runner = do
  let ctx = AuctionState 1000.0
  evalCtx ctx AsAction (Proxy :: Proxy End')               -- we can eval a 'data' type
  evalCtx ctx AsAction (Proxy :: Proxy End)                -- we can eval this as (act ~ MkAction act2, Eval AsAction act2 ())
  evalCtx ctx AsAction (Proxy :: Proxy (Action (GetPayloadValue MinBid Float Drop) End))
  evalCtx ctx AsAction
    ( Proxy :: Proxy
      ( Action (GetPayloadValue MinBid Float Drop)
        ( Action (GetPayloadValue MinBid Float Drop) End)
      )
    )

  evalCtx ctx AsLotProcess
    ( Proxy :: Proxy
      ( LotProcess
        ( Action (GetPayloadValue MinBid Float Print)
          ( Action (GetPayloadValue MinBid Float Drop) End ) )
      )
    )

  evalCtx ctx AsAuctionFlow
    ( Proxy :: Proxy
      ( AuctionFlow
        ( LotProcess
          ( Action (GetPayloadValue MinBid Float Print)
            ( Action (GetPayloadValue MinBid Float Drop) End ) )
        )
      )
    )




spec :: Spec
spec =
  describe "Type level eDSL Auction" $ do

    it "AuctionInfo test" $ do
      strs <- eval AsInfo (Proxy :: Proxy WorldArtsInfo)
      strs `shouldBe`
        [ "Name: World arts"
        , "Holder: UK Bank"
        ]

    it "Auction Lots test" $ do
      strs <- eval AsLots (Proxy :: Proxy WorldArtsLots)
      strs `shouldBe`
        [ "Lot: 101"
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

    it "Auction test" $ do
      strs <- runAuction (Proxy :: Proxy WorldArtsAuction)
      strs `shouldBe`
        [ "==> Auction! <=="
        , "Name: World arts"
        , "Holder: UK Bank"
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
        , "AuctionFlow"
        , "Lot process"
        , "GetPayloadValue' reached"
        , "GetPayloadValue' reached"
        , "End' reached."
        ]