module TCA3.Types where

import CPrelude

import qualified Data.Vector as V


type Idx = Int
type Coords = (Idx, Idx)
type Torus = Bool
type Expandable = Bool

data Dim2Board cell = Dim2Board
  { cells :: V.Vector (V.Vector cell)
  , xSize :: Idx
  , ySize :: Idx
  -- , isTorus :: Torus
  -- , isExpandable :: Expandable
  }
  deriving Show


data TwoStateCell = Alive | Dead
  deriving (Show, Eq, Ord, Enum)
