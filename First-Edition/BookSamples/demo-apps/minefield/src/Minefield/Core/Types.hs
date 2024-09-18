module Minefield.Core.Types where

import CPrelude

import GHC.TypeLits


type ObjectType = String
type ObjectId = Int

type Pos = (Int, Int)
type Icon = Char

newtype TurnsCount = TurnsCount Int
  deriving (Show, Eq, Ord)
newtype TicksCount = TicksCount Int
  deriving (Show, Eq, Ord)

type IsDirected = Bool

data Direction
  = U | D | L | R
  | UL | UR | DL | DR
  deriving (Show, Eq, Ord, Read)

type PlayerPos = Pos
type ActorPos  = Pos
