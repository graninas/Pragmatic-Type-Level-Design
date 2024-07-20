{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Auction.ActionSpec where

import CPrelude

import qualified Prelude

import Auction.Language.Auction
import Auction.Language.DataActions
import qualified Auction.Implementation.DataActions as Impl
import TypeLevelDSL.Eval
import TypeLevelDSL.Context
import qualified TypeLevelDSL.Context as Ctx

import Test.Hspec
import Auction.Testing.Environment

import GHC.TypeLits
import Data.HList.HList
import qualified Data.Map as Map
import qualified Data.Dynamic as Dyn


type Actions =
  '[ ReadRef Int "val1" (WriteRef Int "val2")
   , ReadRef Int "val2" (WriteRef Int "val1")
   ]

type PrintingNameAction =
  GetLine ((ConcatL "Hello, " (ConcatR PrintF "!")))

type Greetings =
  '[ PrintLine "What is your name?"
   , PrintingNameAction
   ]

-- Instance for tests and demo purposes
instance
  ( Context ctx
  , EvalLambda ctx String Impl.AsImplLambda lam (IO [String])
  ) =>
  Eval ctx Impl.AsImplAction (GetLineImpl lam) (IO [String]) where
  eval ctx _ _ = do
    -- line <- getLine      -- disabled; demo only
    let line = "John Doe"
    evalLambda ctx line Impl.AsImplLambda (Proxy :: Proxy lam)

instance
  ( KnownSymbol str
  ) =>
  Eval TestData Impl.AsImplAction
    (PrintLineImpl str)
    (IO [String]) where
  eval ctx _ _ = pure [symbolVal $ Proxy @str]

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

    it "Combinators test" $ do
      ctx <- TestData
              <$> newIORef Map.empty
              <*> pure Map.empty

      lines <- eval ctx Impl.AsImplAction (Proxy @Greetings)

      lines `shouldBe`
        [ "What is your name?"
        , "\"Hello, John Doe!\""
        ]
