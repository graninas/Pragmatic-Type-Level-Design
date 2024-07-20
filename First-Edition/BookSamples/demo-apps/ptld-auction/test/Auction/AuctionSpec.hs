{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}

module Auction.AuctionSpec where

import Auction.Language
import qualified Auction.Implementation as Impl
import qualified Auction.Introspection as I
import qualified Auction.Extensions.Language as ExtL
import qualified Auction.Extensions.Introspection as I
import qualified Auction.Extensions.Implementation as Impl
import Auction.Testing.Environment

import TypeLevelDSL.Eval
import TypeLevelDSL.Context
import qualified TypeLevelDSL.Dyn as Dyn

import Data.HList.HList
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Dynamic as Dyn
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)
import Control.Monad (void)

import Test.Hspec


-- Test sample

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

type UKOnly  = ExtL.AllowedCountries "UK only" '[UK]
type UKAndUS = ExtL.AllowedCountries "UK & US" '[UK, US]

type MinBid202 = 'ValNameS "202 min bid"
type PayloadLot1 = ExtL.EFLotPayload (ExtL.MoneyVal "1000")
type PayloadLot2 = ExtL.EFLotPayload (ExtL.MoneyDynVal MinBid202)
type PayloadLot3 = ExtL.EFLotPayload (ExtL.MoneyVal "40000")

-- Auction algorithm

type EnglishAuctionLotAction1 =
  '[ GetLotName (ConcatL "New lot: " PrintF)
   , GetLotDescr PrintF
   ]

type EnglishAuctionLotAction2 =
  '[ GetLotName
        (ConcatL "New lot: "
          (Both (WriteRef Symbol "lot name result") PrintF))

   , GetLotDescr (ConcatL "Lot description: "
        (Both (WriteRef Symbol "lot descr result") PrintF))
   ]

type EnglishAuctionFlow = AuctionFlow EnglishAuctionLotAction1

type TestFlow = AuctionFlow
  '[ ReadRef Int "curRound" (ShowF Int PrintF)
   , ReadRef Int "curCost" (ShowF Int PrintF)
   ]

-- Auction

type WorldArtsInfo = Info "World arts" "UK Bank"
type WorldArtsLots =
  '[ Lot "101" "Dali artwork"      PayloadLot1 (Currency ExtL.GBP) UKOnly
   , Lot "202" "Chinese vase"      PayloadLot2 (Currency ExtL.USD) UKAndUS
   , Lot "303" "Ancient mechanism" PayloadLot3 (Currency ExtL.USD) NoCensorship
   ]

type WorldArtsAuction = Auction
  EnglishAuctionFlow
  WorldArtsInfo
  WorldArtsLots

type TestAuction = Auction
  TestFlow
  WorldArtsInfo
  WorldArtsLots

spec :: Spec
spec = do
  describe "Type level eDSL Auction: Introspection" $ do

    it "AuctionInfo test" $ do
      strs <- eval () I.AsIntroInfo $ Proxy @WorldArtsInfo
      strs `shouldBe`
        [ "Name: World arts"
        , "Holder: UK Bank"
        ]

    it "Auction Lots test" $ do
      strs <- eval () I.AsIntroLot $ Proxy @WorldArtsLots
      strs `shouldBe`
        [ "Lot: 101"
        , "Description: Dali artwork"
        , "Minimum bid: 1000"
        , "Currency: GBP"
        , "Eligible participants: UK, "
        , "Lot: 202"
        , "Description: Chinese vase"
        , "Minimum bid: 20000"
        , "Currency: USD"
        , "Eligible participants: UK, US, "
        , "Lot: 303"
        , "Description: Ancient mechanism"
        , "Minimum bid: 40000"
        , "Currency: USD"
        ]

    it "Auction test" $ do
      strs <- I.describeAuction $ Proxy @WorldArtsAuction
      strs `shouldBe`
        [ "==> Auction! <=="
        , "Name: World arts"
        , "Holder: UK Bank"
        , "Lot: 101"
        , "Description: Dali artwork"
        , "Minimum bid: 1000"
        , "Currency: GBP"
        , "Eligible participants: UK, "
        , "Lot: 202"
        , "Description: Chinese vase"
        , "Minimum bid: 20000"
        , "Currency: USD"
        , "Eligible participants: UK, US, "
        , "Lot: 303"
        , "Description: Ancient mechanism"
        , "Minimum bid: 40000"
        , "Currency: USD"
        , "AuctionFlow"
        , "GetPayloadValueImpl reached"
        , "ConcatLImpl reached"
        , "New lot: "
        , "PrintFImpl reached"
        , "GetPayloadValueImpl reached"
        , "PrintFImpl reached"
        ]

  describe "Type level eDSL Auction: Implementation" $ do
    it "eval Action ReadRef WriteRef ReadRef test" $ do
      ctx <- TestData <$> newIORef (Map.fromList
        [ ("ref1", Dyn.toDyn (10 :: Int))
        ]) <*> pure Map.empty

      void $ eval ctx Impl.AsImplAction $ Proxy @(
            '[ ReadRef Int "ref1" (WriteRef Int "ref2")
             ])

      verifyRef ctx "ref1" (10 :: Int)
      verifyRef ctx "ref2" (10 :: Int)

    it "eval Action GetPayloadValue test" $ do
      ctx <- TestData <$> newIORef (Map.fromList
        [ (symbolVal $ Proxy @ExtL.MinBid, Dyn.toDyn (10.0 :: Float))
        ]) <*> pure Map.empty

      void $ eval ctx Impl.AsImplAction $ Proxy @(
            '[ GetPayloadValue ExtL.MinBidTag (ShowF Float PrintF)
             , GetPayloadValue ExtL.MinBidTag (WriteRef Float "f")
             ])

      verifyRef ctx (symbolVal $ Proxy @ExtL.MinBid) (10.0 :: Float)
      verifyRef ctx "f" (10.0 :: Float)

    it "eval EnglishAuctionLotAction1 test (separate actions lot name, lot descr)" $ do
      ctx <- TestData <$> newIORef (Map.fromList
          [ ("lot name", Dyn.toDyn ("101" :: String))
          , ("lot descr", Dyn.toDyn ("Dali artwork" :: String))
          ]) <*> pure Map.empty

      void $ eval ctx Impl.AsImplAction
           $ Proxy @EnglishAuctionLotAction1

      verifyRef ctx "lot name" ("101" :: String)

    it "eval EnglishAuctionLotAction2 test (unified action)" $ do
      ctx <- TestData <$> newIORef (Map.fromList
        [ ("lot name", Dyn.toDyn ("101" :: String))
        , ("lot descr", Dyn.toDyn ("Dali artwork" :: String))
        ]) <*> pure Map.empty

      void $ eval ctx Impl.AsImplAction
           $ Proxy @EnglishAuctionLotAction2

      verifyRef ctx "lot name" ("101" :: String)
      verifyRef ctx "lot descr" ("Dali artwork" :: String)
      verifyRef ctx "lot name result" ("New lot: 101" :: String)
      verifyRef ctx "lot descr result" ("Lot description: Dali artwork" :: String)
