{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Tests.CellularModelSpec where

import CPrelude

import Cellular.Language.Automaton
import Cellular.Extensions.Automaton
import Cellular.Implementation.Introspection
import Cellular.Assets.GameOfLife
import Cellular.Assets.ColoredLife

import GHC.TypeLits

import Test.Hspec


instance Introspect 'Red where
  introspect _ = ["Red"]

instance Introspect 'Green where
  introspect _ = ["Green"]

instance Introspect 'Blue where
  introspect _ = ["Blue"]

instance
  ( Introspect color
  ) =>
  Introspect (ColoredStateImpl color) where
  introspect _ = "ColoredStateImpl" : introspect (Proxy @color)


type TestTransition1 = 'StateTransition D A Neighbors3
type TestTransition2 = 'StateTransition A R (Neighbors2 A)

spec :: Spec
spec = do
  describe "Cellular extensible model tests" $ do
    it "Introspection of a GoL state transition" $ do
      let strs = introspect $ Proxy @TestTransition1
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
      let strs = introspect $ Proxy @TestTransition2
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

