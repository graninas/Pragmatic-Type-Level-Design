{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Auction.ActionSpec where

import CPrelude

import Auction.Language.DataActions
import qualified Auction.Implementation.DataActions as Impl
import TypeLevelDSL.Eval
import TypeLevelDSL.Context
import qualified TypeLevelDSL.Context as Ctx

import Test.Hspec
import Auction.Testing.Environment

import Data.HList.HList
import qualified Data.Map as Map
import qualified Data.Dynamic as Dyn


type Actions =
  '[ ReadRef "val1" Int (WriteRef "val2" Int)
   , ReadRef "val2" Int (WriteRef "val1" Int)
   ]

spec :: Spec
spec = do
  describe "Action test" $ do

    it "Action test" $ do
      ctx <- TestData <$> newIORef (Map.fromList
        [ ("val1", Dyn.toDyn (10 :: Int))
        ]) <*> pure Map.empty

      void $ eval ctx Impl.AsImplAction (Proxy @Actions)

      verifyRef ctx "val1" (10 :: Int)
      verifyRef ctx "val2" (10 :: Int)
