{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module IntegritySpec where

import Prelude hiding ((<>))

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Automaton
import Cellular.Implementation.Algorithm

import Test.Hspec
import Data.Proxy
import GHC.TypeLits
import qualified Data.Map as Map


type Coords = GenericCoords

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


cross, cross2Expected :: Board
cross = Map.fromList
  [ ([0,0],0),([0,1],1),([0,2],0)
  , ([1,0],0),([1,1],1),([1,2],0)
  , ([2,0],0),([2,1],1),([2,2],0)
  ]

cross2Expected = Map.fromList
  [ ([0,0],0),([0,1],0),([0,2],0)
  , ([1,0],1),([1,1],1),([1,2],1)
  , ([2,0],0),([2,1],0),([2,2],0)
  ]

type A = 1
type D = 0

type Alive = 'State "Alive" A
type Dead  = 'State "Dead"  D

type LifeLikeStates =
  '[ Alive
   , Dead
   ]

type Neighbors3  = 'NeighborsCount A '[3  ]
type Neighbors23 = 'NeighborsCount A '[2,3]

type B2S23Step states = 'Step @states ('DefState D)
  '[ 'StateTransition D A Neighbors3
   , 'StateTransition A A Neighbors23
   ]

type B2S23Rule = 'Rule
  @LifeLikeStates
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  (B2S23Step LifeLikeStates)


-- Invalid, won't compile: declared states don't match
-- Couldn't match kind: '[]
--   with: '[ 'State "Alive" A, 'State "Dead" D]
-- type InvalidRule1 = 'Rule
--   @LifeLikeStates
--   "Game of Life"
--   "gol"
--   ('AdjacentsLvl 1)
--   (B2S23Step '[])


-- Invalid, won't compile: declared states don't match
-- Couldn't match kind: '[ 'State "Alive" A, 'State "Dead" D]
--   with: '[]
-- type InvalidRule2 = 'Rule
--   @('[])
--   "Game of Life"
--   "gol"
--   ('AdjacentsLvl 1)
--   (B2S23Step LifeLikeStates)


-- Invalid: empty states
type InvalidRule3 = 'Rule
  @('[])
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  (B2S23Step '[])


-- Invalid: only one state declared
type SingleStates =
  '[ 'State "Single" D
   ]

type InvalidRule4 = 'Rule
  @SingleStates
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  (B2S23Step SingleStates)


-- Invalid: same states found
type SameStates =
  '[ 'State "A" D
   , 'State "D" D
   ]

type InvalidRule5 = 'Rule
  @SameStates
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  (B2S23Step SameStates)


-- Invalid: same names found
type SameStateNames =
  '[ 'State "D" A
   , 'State "D" D
   ]

type InvalidRule6 = 'Rule
  @SameStateNames
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  (B2S23Step SameStateNames)


-- Invalid: unknown default state
type X = 222
type InvalidDefaultStep states = 'Step @states ('DefState X)
  '[ 'StateTransition D A Neighbors3
   , 'StateTransition A A Neighbors23
   ]

type InvalidRule7 = 'Rule
  @LifeLikeStates
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  (InvalidDefaultStep LifeLikeStates)


spec :: Spec
spec = do
  describe "Integrity verification" $ do

    it "Successful verification" $ do
      let world1 = CW cross :: CellWorld B2S23Rule
      let CW board2 = iterateWorld world1
      board2 `shouldBe` cross2Expected

  -- Invalid cases that will compile but ideally
  -- they shouldn't be allowed by the model.
  -- Tests are switched off to indicate they don't
  -- really test anything.
  describe "Invalid cases that compile but shouldn't" $ do
    it "Empty states not allowed" $ do
      pendingWith "Invalid test case that shouldn't even compile"
      let world1 = CW cross :: CellWorld InvalidRule3
      let CW board2 = iterateWorld world1
      board2 `shouldBe` cross2Expected

    it "At least two states" $ do
      pendingWith "Invalid test case that shouldn't even compile"
      let world1 = CW cross :: CellWorld InvalidRule4
      let CW board2 = iterateWorld world1
      board2 `shouldBe` cross2Expected

    it "Two identical states not allowed" $ do
      pendingWith "Invalid test case that shouldn't even compile"
      let world1 = CW cross :: CellWorld InvalidRule5
      let CW board2 = iterateWorld world1
      board2 `shouldBe` cross2Expected

    it "Two identical state names not allowed" $ do
      pendingWith "Invalid test case that shouldn't even compile"
      let world1 = CW cross :: CellWorld InvalidRule6
      let CW board2 = iterateWorld world1
      board2 `shouldBe` cross2Expected

    it "Unknown default state" $ do
      pendingWith "Invalid test case that shouldn't even compile"
      let world1 = CW cross :: CellWorld InvalidRule7
      let CW board2 = iterateWorld world1
      board2 `shouldBe` cross2Expected
