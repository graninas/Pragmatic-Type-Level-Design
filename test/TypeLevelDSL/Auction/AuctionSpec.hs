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
import TypeLevelDSL.Auction.Extensions.Language
import qualified TypeLevelDSL.Auction.Implementation.Types as Impl
import qualified TypeLevelDSL.Auction.Implementation.Description as Impl
import qualified TypeLevelDSL.Auction.Implementation.Auction as Impl
import qualified TypeLevelDSL.Auction.Implementation.Flow as Impl
import qualified TypeLevelDSL.Auction.Implementation.Action as Impl
import qualified TypeLevelDSL.Auction.Implementation.DataActions as Impl
import qualified TypeLevelDSL.Auction.Introspection as I
import qualified TypeLevelDSL.Auction.Extensions.Introspection as I
import qualified TypeLevelDSL.Auction.Extensions.Implementation as Impl
import TypeLevelDSL.Eval
import TypeLevelDSL.Context

import Test.Hspec

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Dynamic as Dyn
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)
import Control.Monad (void)


-- Test sample

type UKOnly  = Censorship (AllowedCountries "UK only" '[UK])
type UKAndUS = Censorship (AllowedCountries "UK & US" '[UK, US])

type PayloadLot1 = LotPayload (Payload (MoneyVal "1000"))
type PayloadLot2 = LotPayload (Payload (MoneyDynVal "202 min bid"))
type PayloadLot3 = LotPayload (Payload (MoneyVal "40000"))

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
  '[ Lot "101" "Dali artwork"      PayloadLot1 (Currency GBP) UKOnly
   , Lot "202" "Chinese vase"      PayloadLot2 (Currency USD) UKAndUS
   , Lot "303" "Ancient mechanism" PayloadLot3 (Currency USD) NoCensorship
   ]

type WorldArtsAuction = Auction
  EnglishAuctionFlow
  WorldArtsInfo
  WorldArtsLots

type TestAuction = Auction
  TestFlow
  WorldArtsInfo
  WorldArtsLots

data TestData = TestData
  { dynsRef :: IORef (Map.Map String Dyn.Dynamic)
  }

instance Context TestData where
  getDyn TestData {dynsRef} refName = do
    dyns <- readIORef dynsRef
    pure $ Map.lookup refName dyns

spec :: Spec
spec = do
  describe "Type level eDSL Auction: Introspection" $ do

    xit "AuctionInfo test" $ do
      strs <- eval I.AsIntroInfo (Proxy :: Proxy WorldArtsInfo)
      strs `shouldBe`
        [ "Name: World arts"
        , "Holder: UK Bank"
        ]

    xit "Auction Lots test" $ do
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

    xit "Auction test" $ do
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
        , "GetPayloadValue' reached"
        , "GetPayloadValue' reached"
        , "End' reached."
        ]

  describe "Type level eDSL Auction: Implementation" $ do
    -- it "runAuction WorldArtsAuction test" $ do
      -- Impl.runAuction (Proxy :: Proxy WorldArtsAuction)

    it "runAuction TestAuction test" $ do
      Impl.runAuction (Proxy :: Proxy TestAuction)

    it "runAction ReadRef WriteRef ReadRef test" $ do
      ctx <- TestData <$> newIORef (Map.fromList [("ref1", Dyn.toDyn (10 :: Int))])

      void $ evalCtx ctx Impl.AsImplAction (Proxy :: Proxy (
            Action (ReadRef "ref1" Int Print)
              ( Action (ReadRef "ref1" Int Drop)
                End
              )
          ))
      pure ()
