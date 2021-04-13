module HCell.Game.Logic where

import HCell.Prelude

import HCell.Types

import qualified Data.Set as Set


golStep :: Level -> Level
golStep level = newCells2
  where
    allNeighbours = Set.foldr' mkNeighbours Set.empty level
    inactiveNeighbours = Set.difference allNeighbours level
    newCells1 = Set.foldr' (golRule level) Set.empty level
    newCells2 = Set.foldr' (golRule level) newCells1 inactiveNeighbours


golRule
  :: Level
  -> Coords -> Level -> Level
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
