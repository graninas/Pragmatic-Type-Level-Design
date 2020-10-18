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

import           Test.Hspec

import           Data.Proxy (Proxy(..))
import           GHC.TypeLits (Symbol)


data USD
data EUR
data GBP

data AllowedCountries (name :: Symbol) (participants :: [ Country ])


-- Test sample

type UKOnly = Censorship (AllowedCountries "UK only" '[UK])
type UKAndUS = Censorship (AllowedCountries "UK only" '[UK, US])
type Info1 = AuctionInfo (Info "UK Art" EnglishAuction "UK Bank")
type Lot1 = Lot "101" "Dali artwork" (Currency GBP) UKOnly
type Lot2 = Lot "202" "Chinesse vase" (Currency USD) UKAndUS
type Lot3 = Lot "303" "Ancient mechanism" (Currency USD) NoCensorship
type Auction1 = Auction Info1 (Lots '[Lot1, Lot2, Lot3])


runner :: IO ()
runner = pure ()


spec :: Spec
spec =
  describe "Type level Servant-like eDSL Auction" $ do
    it "Run Auction script" $ do
      runner
