module Minefield.Core.Types where

import CPrelude

import GHC.TypeLits


type ObjectType = String
type Pos = (Int, Int)
type Icon = Char

newtype TurnsCount = TurnsCount Int
  deriving (Show, Eq, Ord)
newtype TicksCount = TicksCount Int
  deriving (Show, Eq, Ord)
