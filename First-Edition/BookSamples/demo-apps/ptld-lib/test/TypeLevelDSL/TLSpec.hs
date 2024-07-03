{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TypeLevelDSL.TLSpec where

import CPrelude

import TypeLevelDSL.Language
import TypeLevelDSL.Eval
import TypeLevelDSL.Context
import qualified TypeLevelDSL.Implementation as Impl
import qualified TypeLevelDSL.Context as Ctx

import Test.Hspec
import TypeLevelDSL.Testing.Environment

import qualified Data.Map as Map
import qualified Data.Dynamic as Dyn


type Actions =
  '[ ReadRef "val1" Int (WriteRef "val2" Int)
   ]

spec :: Spec
spec = do
  describe "Eval test" $ do

    it "Eval test" $ do
      ctx <- TestData <$> newIORef (Map.fromList
        [ ("val1", Dyn.toDyn (10 :: Int))
        ]) <*> pure Map.empty

      void $ evalCtx ctx Impl.AsImplActions (Proxy @Actions)

      verifyRef ctx "val1" (10 :: Int)
      verifyRef ctx "val2" (10 :: Int)
