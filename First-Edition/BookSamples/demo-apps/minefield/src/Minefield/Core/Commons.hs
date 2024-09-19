module Minefield.Core.Commons where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object


-- Explosion icons. Should indicate how many ticks to live.
explosionIcons :: [OverhaulIcon]
explosionIcons =
  [ OverhaulIcon Nothing '*'  (Just 2)
  , OverhaulIcon Nothing '|'  (Just 2)
  , OverhaulIcon Nothing '/'  (Just 2)
  , OverhaulIcon Nothing '-'  (Just 2)
  , OverhaulIcon Nothing '\\' (Just 2)
  ]


movePos :: Pos -> Direction -> Pos
movePos (x, y) U = (x, y-1)
movePos (x, y) D = (x, y+1)
movePos (x, y) L = (x-1, y)
movePos (x, y) R = (x+1, y)
movePos pos DL = movePos (movePos pos L) D
movePos pos UR = movePos (movePos pos R) U
movePos pos UL = movePos (movePos pos L) U
movePos pos DR = movePos (movePos pos R) D
