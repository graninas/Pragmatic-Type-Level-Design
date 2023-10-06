{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
module AutoNGSpec where

import Domain.AutomatonNG

import Test.Hspec
import Data.Proxy
import qualified Data.Map as Map


spec :: Spec
spec =
  describe "Automata eDSL tests" $ do
    it "2 state board init" $ do
      let CW board = initBoard :: CellWorld GameOfLife
      board `shouldBe` Map.empty
    it "2 state board neighbors" $ do
      let world = initBoard :: CellWorld GameOfLife

      let ns = neighbors
                [0,0]
                (AdjacentsLvl 1)
                world

      ns `shouldBe` [ ([-1,-1],0),([-1,0],0),([-1,1],0)
                    , ([0,-1],0)            ,([0,1],0)
                    , ([1,-1],0) ,([1,0],0) ,([1,1],0)]


