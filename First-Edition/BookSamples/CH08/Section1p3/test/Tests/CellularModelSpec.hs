{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module Tests.CellularModelSpec where

import CPrelude

import TypeLevelDSL.Eval
import Cellular.Language.Automaton
import Cellular.Extensions.Automaton
import Cellular.Implementation.Introspection
import Cellular.Assets.GameOfLife
import Cellular.Assets.ColoredLife

import GHC.TypeLits

import Test.Hspec


instance
  Eval () Introspect 'Red [String] where
  eval _ _ _ = ["Red"]

instance
  Eval () Introspect 'Green [String] where
  eval _ _ _ = ["Green"]

instance
  Eval () Introspect 'Blue [String] where
  eval _ _ _ = ["Blue"]

instance
  ( Eval () Introspect color [String]
  ) =>
  Eval () Introspect (ColoredStateImpl color) [String] where
  eval _ _ _ = "ColoredStateImpl" : eval () Introspect (Proxy @color)


type TestTransition1 = 'StateTransition D A Neighbors3
type TestTransition2 = 'StateTransition A R (Neighbors2 A)

spec :: Spec
spec = do
  describe "Cellular extensible model tests" $ do
    it "Introspection of a GoL state transition" $ do
      let strs = eval () Introspect $ Proxy @TestTransition1
      strs `shouldBe`
        [ "StateTransition"
        , "StateImpl"
        , "Dead"
        , "0"
        , "StateImpl"
        , "Alive"
        , "1"
        , "NeighborsCountImpl"
        , "StateImpl"
        , "Alive"
        , "1"
        , "3"
        ]
    it "Introspection of a ColoredLife state transition" $ do
      let strs = eval () Introspect $ Proxy @TestTransition2
      strs `shouldBe`
        [ "StateTransition"
        , "StateImpl"
        , "Alive"
        , "1"
        , "ColoredStateImpl"
        , "Red"
        , "NeighborsCountImpl"
        , "StateImpl"
        , "Alive"
        , "1"
        , "2"
        ]

