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


fillBoard2Dim
  :: GenericCoords
  -> GenericCoords
  -> Board
  -> Board
fillBoard2Dim [fromX, fromY] [toX, toY] board =
  let newCells = [[x, y]
        | x <- [fromX .. toX]
        , y <- [fromY .. toY]]
  in foldr (\k -> Map.insertWith (\_ a -> a) k 0) board newCells



glider :: Board
glider = Map.fromList [([1, 0], 1),
                       ([2, 1], 1),
                       ([0, 2], 1),
                       ([1, 2], 1),
                       ([2, 2], 1)]

gol1 :: Board
gol1 = fillBoard2Dim [-1, -1] [3, 3] glider

gol2Expected :: Board
gol2Expected = Map.fromList
    [ ([-1,-1], 0), ([-1,0], 0),  ([-1,1], 0)
    , ([-1,2],  0), ([-1,3], 0),  ([0,-1], 0)
    , ([0,0],   0), ([0,1],  1),  ([0,2],  0)
    , ([0,3],   0), ([1,-1], 0),  ([1,0],  0)
    , ([1,1],   0), ([1,2],  1),  ([1,3],  1)
    , ([2,-1],  0), ([2,0],  0),  ([2,1],  1)
    , ([2,2],   1), ([2,3],  0),  ([3,-1], 0)
    , ([3,0],   0), ([3,1],  0),  ([3,2],  0)
    , ([3,3],   0)]

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
      let world1 = CW gol1 :: CellWorld GameOfLife
      let CW board2 = iterateWorld world1

      board2 `shouldBe` gol2Expected
