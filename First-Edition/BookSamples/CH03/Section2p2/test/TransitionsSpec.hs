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

type Neighbors3  = 'NeighborsCount A '[3  ]
type Neighbors23 = 'NeighborsCount A '[2,3]

type B2S23Transitions =
  '[ 'StateTransition D A Neighbors3
   , 'StateTransition A A Neighbors23
   , 'DefaultTransition D
   ]

class ApplyTransition (t :: CustomStateTransition) where
  applyTransition
    :: Proxy t
    -> Board
    -> Coords
    -> Maybe Nat


class EvaluateTransitions (tsList :: [ts]) where
  evaluateTransitions
    :: Proxy tsList
    -> Board
    -> Coords
    -> Int
    -> Int

instance ApplyTransition
  ('StateTransition x y ts) where
  applyTransition _ _ _ = undefined

instance ApplyTransition
  ('DefaultTransition x) where
  applyTransition _ _ _ = undefined



instance EvaluateTransitions '[] where
  evaluateTransitions _ _ _ oldCell = oldCell  -- FIXME: default case

instance
  ( EvaluateTransitions ts
  , ApplyTransition t
  ) =>
  EvaluateTransitions (t ': ts) where
  evaluateTransitions _ board coords oldCell =
    case applyTransition (Proxy @t) board coords of
      Nothing -> evaluateTransitions (Proxy @ts) board coords oldCell
      Just newCell -> fromIntegral newCell

evaluateTransitions'
    :: EvaluateTransitions ts
    => Board
    -> Proxy ts
    -> Board
evaluateTransitions' board proxy =
  Map.mapWithKey (evaluateTransitions proxy board) board

spec :: Spec
spec = do
  describe "Case-driven design" $ do
    it "Cross test case" $ do
      pendingWith "Incomplete functionality"
      let cross2 = evaluateTransitions' cross (Proxy @B2S23Transitions)
      cross2 `shouldBe` cross2Expected
