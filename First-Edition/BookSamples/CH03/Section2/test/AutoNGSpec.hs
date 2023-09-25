{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module AutoNGSpec where

import Domain.BoardNG
import Domain.CellTransitionNG
import Domain.AutomatonNG

import Test.Hspec
import Data.Proxy
import qualified Data.Map as Map


-- -------------------------------------------------


type Open2StateBoard = SquareGrid Open         -- Type application to types


type GoLStep = 'Step
  '[ 'StateTransition 0 1 '[ 'CellsCount 1 '[3 ]]   -- "Born rule"
   , 'StateTransition 1 1 '[ 'CellsCount 1 '[2,3]]  -- "Survive rule"
   , 'DefaultTransition 0
   ]

type GameOfLife = 'Rule
  "Game of Life"
  "gol"
  Open2StateBoard
  ('AdjacentsLvl 1)
  GoLStep



instance IWorld GameOfLife where

instance IAutomaton GameOfLife where
  step
    :: CellWorld GameOfLife
    -> CellWorld GameOfLife
  step (CW b) = let
    -- updCellFunc = makeUpdateFunc
    in CW b




spec :: Spec
spec =
  describe "Automata eDSL tests" $ do
    it "2 state board init" $ do
      let CW board = initWorld :: CellWorld GameOfLife
      board `shouldBe` Map.empty
    it "2 state board neighbors" $ do
      let CW board = initWorld :: CellWorld GameOfLife

      let ns = neighbors
                [0,0]
                (AdjacentsLvl 1)
                board

      ns `shouldBe` [ ([-1,-1],0),([-1,0],0),([-1,1],0)
                    , ([0,-1],0)            ,([0,1],0)
                    , ([1,-1],0) ,([1,0],0) ,([1,1],0)]

    it "Apply type-level step" $ do
      let world1 = initWorld :: CellWorld GameOfLife
      let CW board2 = iterateWorld world1

      board2 `shouldBe` Map.empty
