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
import Cellular.Language.Integrity
import Cellular.Automaton
import Cellular.Implementation.Algorithm
import Common.NonEmptyList

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

type LifeLikeStates = 'List2
  Alive
  Dead
  '[]

-- N.B., complexity has increased
type Neighbors3  = 'NeighborsCount A ('List1 3 '[])
type Neighbors23 = 'NeighborsCount A ('List1 2 '[3])

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
-- type InvalidRule1 = 'Rule
--   @LifeLikeStates
--   "Game of Life"
--   "gol"
--   ('AdjacentsLvl 1)
--   (B2S23Step '[])


-- Invalid, won't compile: declared states don't match
-- type InvalidRule2 = 'Rule
--   @('[])
--   "Game of Life"
--   "gol"
--   ('AdjacentsLvl 1)
--   (B2S23Step LifeLikeStates)


-- No way to construct: empty states
-- type InvalidRule3 = 'Rule
--   @('[])
--   "Game of Life"
--   "gol"
--   ('AdjacentsLvl 1)
--   (B2S23Step '[])


-- No way to construct: only one state declared
-- type SingleStates =
--   '[ 'State "Single" D
--    ]

-- type InvalidRule4 = 'Rule
--   @SingleStates
--   "Game of Life"
--   "gol"
--   ('AdjacentsLvl 1)
--   (B2S23Step SingleStates)


-- Invalid: same states found
type SameStates = 'List2
  ('State "A" D)
  ('State "D" D)
  '[]

type InvalidRule5 = 'Rule
  @SameStates
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  (B2S23Step SameStates)

-- Invalid: same states found (test case 2)
type SameStates_2 = 'List2
  ('State "A" A)
  ('State "D" D)
  '[ 'State "X" 222
   , 'State "Y" A
   ]

type InvalidRule5_2 = 'Rule
  @SameStates_2
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  (B2S23Step SameStates_2)


-- Invalid: same names found
type SameStateNames = 'List2
  ('State "D" A)
  ('State "D" D)
  '[]

type InvalidRule6 = 'Rule
  @SameStateNames
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  (B2S23Step SameStateNames)


-- Invalid: same names found
type SameStateNames_2 = 'List2
  ('State "A" A)
  ('State "D" D)
  '[ 'State "X" 222
   , 'State "D" 333
   ]

type InvalidRule6_2 = 'Rule
  @SameStateNames_2
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  (B2S23Step SameStateNames_2)


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

    -- No way to construct:
    -- it "Empty states not allowed" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule3
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected

    -- No way to construct:
    -- it "At least two states" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule4
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected

    -- Won't compile (has a bad compiler message):
    --   Couldn't match type ‘'True’ with ‘'False’
    -- it "Two identical states not allowed (case 1)" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule5
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected

    -- Won't compile (has a bad compiler message):
    --   Couldn't match type ‘'True’ with ‘'False’
    -- it "Two identical states not allowed (case 2)" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule5_2
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected

    -- Won't compile (has a bad compiler message):
    --   Couldn't match type ‘'True’ with ‘'False’
    -- it "Two identical state names not allowed (case 1)" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule6
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected

    -- Won't compile (has a bad compiler message):
    --   Couldn't match type ‘'True’ with ‘'False’
    -- it "Two identical state names not allowed (case 2)" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule6_2
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected

    -- Won't compile (has a bad compiler message):
    --   Couldn't match type ‘'True’ with ‘'False’
    -- it "Unknown default state" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule7
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected
