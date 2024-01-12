{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module TransitionsSpec where

import Prelude hiding ((<>))

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton
import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.LifeLike


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

type B2S23Step states = 'Step @states ('DefState D)
  '[ 'StateTransition D A ('NeighborsCount A '[3  ])
   , 'StateTransition A A ('NeighborsCount A '[2,3])
   ]

type B2S23Rule = 'Rule
  @LifeLikeStates
  "Game of Life"
  "gol"
  ('AdjacentsLvl 1)
  (B2S23Step LifeLikeStates)

class ApplyTransition (t :: CustomStateTransition) where
  applyTransition
    :: Proxy t
    -> Board
    -> Coords
    -> Int           -- Old state
    -> Maybe Nat


class EvaluateTransitions (tsList :: [ts]) where
  evaluateTransitions
    :: Proxy tsList
    -> Board
    -> Int        -- Default state
    -> Coords
    -> Int        -- Old state
    -> Int        -- New state

class EvaluateStep (step :: CustomStep (states :: [CustomState])) where
  evaluateStep
    :: Proxy step
    -> Board
    -> Board


instance ApplyTransition
  ('StateTransition x y ts) where
  applyTransition _ _ _ _ = undefined

instance EvaluateTransitions '[] where
  evaluateTransitions _ _ def _ _ = def

instance
  ( EvaluateTransitions ts
  , ApplyTransition t
  ) =>
  EvaluateTransitions (t ': ts) where
  evaluateTransitions _ board def coords old =
    case applyTransition (Proxy @t) board coords old of
      Nothing -> evaluateTransitions (Proxy @ts) board def coords old
      Just new -> fromIntegral new

instance
  ( EvaluateTransitions ts
  , KnownNat def
  ) =>
  EvaluateStep ('Step ('DefState def) ts) where
  evaluateStep proxy board =
    let def = fromIntegral $ natVal $ Proxy @def
    in Map.mapWithKey (evaluateTransitions (Proxy @ts) board def) board



spec :: Spec
spec = do
  describe "Case-driven design" $ do
    it "Cross test case" $ do
      pendingWith "Incomplete functionality"
      let cross2 = evaluateStep (Proxy @(B2S23Step LifeLikeStates)) cross
      cross2 `shouldBe` cross2Expected
