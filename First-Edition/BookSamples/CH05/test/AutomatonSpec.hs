{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module AutomatonSpec where

import Prelude hiding ((<>))

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Automaton
import Cellular.Implementation.Algorithm
import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.LifeLike


import Test.Hspec
import Data.Proxy
import GHC.TypeLits
import qualified Data.Map as Map


-- -------------------------------------------------
neighbors
  :: GenericCoords
  -> Neighborhood
  -> Board
  -> Cells
neighbors coords ns board = let
  ns' = generateNeighborhood coords ns
  in getCells ns' 0 board

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
spec = do
  describe "Automata eDSL tests" $ do
    it "2 state board init" $ do
      let CW board = initWorld () :: CellWorld GoLRule
      board `shouldBe` Map.empty
    it "2 state board neighbors" $ do
      let CW board = initWorld () :: CellWorld GoLRule

      let ns = neighbors
                [0,0]
                (AdjacentsLvl 1)
                board

      ns `shouldBe` [ ([-1,-1],0),([-1,0],0),([-1,1],0)
                    , ([0,-1],0)            ,([0,1],0)
                    , ([1,-1],0) ,([1,0],0) ,([1,1],0)]

    it "Apply type-level step" $ do
      let world1 = CW gol1 :: CellWorld GoLRule
      let CW board2 = iterateWorld world1

      board2 `shouldBe` gol2Expected
