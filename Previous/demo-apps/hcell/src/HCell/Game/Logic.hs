module HCell.Game.Logic where

import CPrelude

import HCell.Types

import qualified Data.Set as Set

-- FIXME: this GoL logic has bugs.

golStep :: AliveCells -> AliveCells
golStep cells = newCells2
  where
    allNeighbours = Set.foldr' mkNeighbours Set.empty cells
    inactiveNeighbours = Set.difference allNeighbours cells
    newCells1 = Set.foldr' (golRule cells) Set.empty cells
    newCells2 = Set.foldr' (golRule cells) newCells1 inactiveNeighbours


golRule
  :: AliveCells
  -> Coords -> AliveCells -> AliveCells
golRule oldLvl (x, y) newLvl = res
  where
    neighbours = filter (==True)
      [ Set.member (x-1, y-1) oldLvl
      , Set.member (x,   y-1) oldLvl
      , Set.member (x+1, y-1) oldLvl
      , Set.member (x-1, y)   oldLvl
      , Set.member (x+1, y)   oldLvl
      , Set.member (x-1, y+1) oldLvl
      , Set.member (x,   y+1) oldLvl
      , Set.member (x+1, y+1) oldLvl
      ]
    res = case length neighbours of
      2 -> Set.insert (x, y) newLvl
      3 -> Set.insert (x, y) newLvl
      _ -> newLvl


mkNeighbours (x, y) s = foldr Set.insert s
  [ (x-1, y-1)
  , (x,   y-1)
  , (x+1, y-1)
  , (x-1, y)
  , (x+1, y)
  , (x-1, y+1)
  , (x,   y+1)
  , (x+1, y+1)
  ]
