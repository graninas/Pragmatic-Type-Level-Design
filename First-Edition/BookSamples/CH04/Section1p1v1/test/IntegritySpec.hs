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
import Cellular.Assets.Automata.Boards
import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.LifeLike
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

type States2 =
  '[ 'State "Alive" A
   , 'State "Dead"  D
   ]

type B2S23Step states = 'Step @states ('DefState D)
  '[ 'StateTransition D A ('NeighborsCount A '[3  ])
   , 'StateTransition A A ('NeighborsCount A '[2,3])
   ]

type B2S23Rule = 'Rule
  @States2
  "Game of Life"
  "gol"
  OpenBoard
  ('AdjacentsLvl 1)
  (B2S23Step States2)


-- Invalid, won't compile: declared states don't match
-- Couldn't match kind: '[]
--   with: '[ 'State "Alive" A, 'State "Dead" D]
-- type InvalidRule1 = 'Rule
--   @States2
--   "Game of Life"
--   "gol"
--   OpenBoard
--   ('AdjacentsLvl 1)
--   (B2S23Step '[])


-- Invalid, won't compile: declared states don't match
-- Couldn't match kind: '[ 'State "Alive" A, 'State "Dead" D]
--   with: '[]
-- type InvalidRule2 = 'Rule
--   @('[])
--   "Game of Life"
--   "gol"
--   OpenBoard
--   ('AdjacentsLvl 1)
--   (B2S23Step States2)


-- Invalid: empty states
type InvalidRule3 = 'Rule
  @('[])
  "Game of Life"
  "gol"
  OpenBoard
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
  OpenBoard
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
  OpenBoard
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
  OpenBoard
  ('AdjacentsLvl 1)
  (B2S23Step SameStateNames)


-- Invalid: unknown default state
type X = 222
type InvalidDefaultStep states = 'Step @states ('DefState X)
  '[ 'StateTransition D A ('NeighborsCount A '[3  ])
   , 'StateTransition A A ('NeighborsCount A '[2,3])
   ]

type InvalidRule7 = 'Rule
  @States2
  "Game of Life"
  "gol"
  OpenBoard
  ('AdjacentsLvl 1)
  (InvalidDefaultStep States2)


spec :: Spec
spec = do
  describe "Integrity verification" $ do

    it "Successful verification" $ do
      let world1 = CW cross :: CellWorld B2S23Rule
      let CW board2 = iterateWorld world1
      board2 `shouldBe` cross2Expected

    -- Won't compile:
    --   No instance for (Verify (StatesNotEmpty '[]))
    --   arising from a use of ‘iterateWorld’
    -- it "Empty states not allowed" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule3
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected

    -- Won't compile:
    --   No instance for (Verify (AtLeastTwoStates '[ 'State "Single" D]))
    -- it "At least two states" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule4
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected

    -- Won't compile (has a bad compiler message):
    --   Couldn't match type ‘'True’ with ‘'False’
    -- it "Two identical states not allowed" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule5
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected

    -- Won't compile (has a bad compiler message):
    --   Couldn't match type ‘'True’ with ‘'False’
    -- it "Two identical state names not allowed" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule6
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected

    -- Won't compile (has a bad compiler message):
    --   Couldn't match type ‘'True’ with ‘'False’
    -- it "Unknown default state" $ do
    --   let world1 = CW cross :: CellWorld InvalidRule7
    --   let CW board2 = iterateWorld world1
    --   board2 `shouldBe` cross2Expected
