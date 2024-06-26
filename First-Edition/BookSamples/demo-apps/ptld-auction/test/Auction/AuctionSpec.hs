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
import qualified Auction.Implementation.Description as Impl
import qualified Auction.Implementation.Auction as Impl
import qualified Auction.Implementation.Flow as Impl
import qualified Auction.Implementation.DataActions as Impl
import qualified Auction.Introspection as I
import qualified Auction.Extensions.Language as ExtL
import qualified Auction.Extensions.Introspection as I
import qualified Auction.Extensions.Implementation as Impl

import TypeLevelDSL.Eval
import TypeLevelDSL.Context
import TypeLevelDSL.Language
import qualified TypeLevelDSL.Implementation as Impl
import qualified TypeLevelDSL.Dyn as Dyn

import TypeLevelDSL.Testing.Environment

import Test.Hspec

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Dynamic as Dyn
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)
import Control.Monad (void)


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

type UKOnly  = Censorship (ExtL.AllowedCountries "UK only" '[UK])
type UKAndUS = Censorship (ExtL.AllowedCountries "UK & US" '[UK, US])

type MinBid202 = 'ValNameS "202 min bid"
type PayloadLot1 = LotPayload (ExtL.EFLotPayload (MoneyVal "1000"))
type PayloadLot2 = LotPayload (ExtL.EFLotPayload (MoneyDynVal MinBid202))
type PayloadLot3 = LotPayload (ExtL.EFLotPayload (MoneyVal "40000"))

-- Auction algorithm

type EnglishAuctionLotAction1 =
  ( Action (GetLotName (ConcatL "New lot: " Print))
  ( Action (GetLotDescr Print)
    End
  ))

type EnglishAuctionLotAction2 =
  ( Action (GetLotName2  (ConcatL "New lot: "         (Both (WriteRef "LotName result" String)  Print)))
  ( Action (GetLotDescr2 (ConcatL "Lot description: " (Both (WriteRef "LotDescr result" String) Print)))
    End
  ))

type EnglishAuctionFlow = AuctionFlow
  ( LotProcess EnglishAuctionLotAction1
  )


type TestFlow = AuctionFlow
  ( LotProcess
      ( Action (ReadRef "curRound" Int Print)
        ( Action (ReadRef "curCost" Int Drop)
          End
        )
      )
  )

-- Auction

type WorldArtsInfo = Info "World arts" "UK Bank"
type WorldArtsLots = Lots
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
      strs <- eval I.AsIntroInfo (Proxy :: Proxy WorldArtsInfo)
      strs `shouldBe`
        [ "Name: World arts"
        , "Holder: UK Bank"
        ]

    it "Auction Lots test" $ do
      strs <- eval I.AsIntroLots (Proxy :: Proxy WorldArtsLots)
      strs `shouldBe`
        [ "Lot: 101"
        , "Description: Dali artwork"
        , "Minimum bid: 1000"
        , "Currency: GBP"
        , "Eligible participants: UK"
        , "Lot: 202"
        , "Description: Chinese vase"
        , "Minimum bid: 20000"
        , "Currency: USD"
        , "Eligible participants: UK, US"
        , "Lot: 303"
        , "Description: Ancient mechanism"
        , "Minimum bid: 40000"
        , "Currency: USD"
        ]

    it "Auction test" $ do
      strs <- I.describeAuction (Proxy :: Proxy WorldArtsAuction)
      strs `shouldBe`
        [ "==> Auction! <=="
        , "Name: World arts"
        , "Holder: UK Bank"
        , "Lot: 101"
        , "Description: Dali artwork"
        , "Minimum bid: 1000"
        , "Currency: GBP"
        , "Eligible participants: UK"
        , "Lot: 202"
        , "Description: Chinese vase"
        , "Minimum bid: 20000"
        , "Currency: USD"
        , "Eligible participants: UK, US"
        , "Lot: 303"
        , "Description: Ancient mechanism"
        , "Minimum bid: 40000"
        , "Currency: USD"
        , "AuctionFlow"
        , "Lot process"
        , "GetLotName' reached"
        , "GetLotDescr' reached"
        , "End' reached."
        ]

  describe "Type level eDSL Auction: Implementation" $ do
    it "evalCtx Action ReadRef WriteRef ReadRef test" $ do
      ctx <- TestData <$> newIORef (Map.fromList
        [ ("ref1", Dyn.toDyn (10 :: Int))
        ]) <*> pure Map.empty

      void $ evalCtx ctx Impl.AsImplAction (Proxy :: Proxy (
            Action (ReadRef "ref1" Int (WriteRef "ref2" Int))
              ( Action (ReadRef "ref2" Int Drop)
                End
              )
          ))

      verifyRef ctx "ref1" (10 :: Int)
      verifyRef ctx "ref2" (10 :: Int)

    it "evalCtx Action GetPayloadValue test" $ do
      ctx <- TestData <$> newIORef (Map.fromList
        [ (Dyn.toTypeableKey @MinBid, Dyn.toDyn (10.0 :: Float))
        ]) <*> pure Map.empty

      void $ evalCtx ctx Impl.AsImplAction (Proxy :: Proxy (
            ( Action (GetPayloadValue MinBid Float Print)
              ( Action (GetPayloadValue MinBid Float (WriteRef "f" Float))
                End
              )
            )
          ))

      verifyRef ctx (Dyn.toTypeableKey @MinBid) (10.0 :: Float)
      verifyRef ctx "f" (10.0 :: Float)

    it "evalCtx EnglishAuctionLotAction1 test (separate actions LotName LotDescr)" $ do
      ctx <- TestData <$> newIORef (Map.fromList
          [ ("LotName", Dyn.toDyn ("101" :: String))
          , ("LotDescr", Dyn.toDyn ("Dali artwork" :: String))
          ]) <*> pure Map.empty

      void $ evalCtx ctx Impl.AsImplAction (Proxy :: Proxy EnglishAuctionLotAction1)

      verifyRef ctx "LotName" ("101" :: String)

    it "evalCtx EnglishAuctionLotAction2 test (unified action)" $ do
      ctx <- TestData <$> newIORef (Map.fromList
        [ ("LotName", Dyn.toDyn ("101" :: String))
        , ("LotDescr", Dyn.toDyn ("Dali artwork" :: String))
        ]) <*> pure Map.empty

      void $ evalCtx ctx Impl.AsImplAction (Proxy :: Proxy EnglishAuctionLotAction2)

      verifyRef ctx "LotName" ("101" :: String)
      verifyRef ctx "LotDescr" ("Dali artwork" :: String)
      verifyRef ctx "LotName result" ("New lot: 101" :: String)
      verifyRef ctx "LotDescr result" ("Lot description: Dali artwork" :: String)
