{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.AutomatonNG where


import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Domain.Cell (Cell)


data Topology = Open | Torus
type Dimensions = Nat

type RuleCode = String
type Name = Symbol
type States = Nat

data Neighborhood where
  Adjacents :: Neighborhood

data CustomBoard where
  RegularBoard    -- names of val constr should differ
                  -- to avoid name clash (the compiler gets confused)
    :: Topology
    -> Dimensions
    -> States
    -> CustomBoard

data CustomRule (board :: CustomBoard) where
  Rule
    :: Name
    -> Neighborhood
    -> CustomRule board

data CellWorld (board :: CustomBoard)

class IBoard (board :: CustomBoard) where


class IAutomaton (board :: CustomBoard) (rule :: CustomRule board) where
  step
    :: Proxy rule
    -> CellWorld board
    -> CellWorld board

  init :: Proxy rule -> CellWorld board


-- TODO: research the difference
-- class IAutomaton board rule where
  -- step
  --   :: Proxy (rule :: CustomRule board)
  --   -> CellWorld (board :: CustomBoard)
  --   -> CellWorld (board :: CustomBoard)

data CustomStateTrans where
  StateTrans
    :: Nat    -- from state
    -> Nat    -- to state
    -> [Nat]  -- neighbors count conditions
    -> CustomStateTrans

data CustomStep where
  Step :: [CustomStateTrans] -> CustomStep

data Lines3S
type Lines3SBoard = CustomBoard

type LifeLikeBoard = RegularBoard Open 2 2
type GoLStep = Step
  '[ StateTrans 1 1 '[2,3]  -- "Survive rule"
   , StateTrans 0 1 '[3]    -- "Born rule"
   ]
type GameOfLife = Rule "Game of Life" Adjacents

instance IAutomaton LifeLikeBoard GameOfLife where
  step
    :: Proxy GameOfLife
    -> CellWorld LifeLikeBoard
    -> CellWorld LifeLikeBoard
  step _ b = b

  init :: Proxy GameOfLife -> CellWorld LifeLikeBoard
  init _ = undefined
