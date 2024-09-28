module Minefield.Core.Commons where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.Defaults


-- Explosion icons. Should indicate how many ticks to live.
explosionIcons :: [OverhaulIcon]
explosionIcons =
  [ OverhaulIcon Nothing '*'  (Just 2)
  , OverhaulIcon Nothing '|'  (Just 2)
  , OverhaulIcon Nothing '/'  (Just 2)
  , OverhaulIcon Nothing '-'  (Just 2)
  , OverhaulIcon Nothing '\\' (Just 2)
  ]

disarmedIcon :: Icon
disarmedIcon = '!'

disarmedOverhaulIcon :: OverhaulIcon
disarmedOverhaulIcon = OverhaulIcon Nothing disarmedIcon Nothing

deadMineOverhaulIcon :: OverhaulIcon
deadMineOverhaulIcon = OverhaulIcon Nothing 'x' Nothing

movePos :: Pos -> Direction -> Pos
movePos (x, y) U = (x, y-1)
movePos (x, y) D = (x, y+1)
movePos (x, y) L = (x-1, y)
movePos (x, y) R = (x+1, y)
movePos pos DL = movePos (movePos pos L) D
movePos pos UR = movePos (movePos pos R) U
movePos pos UL = movePos (movePos pos L) U
movePos pos DR = movePos (movePos pos R) D

neighboursLvl2 (x, y) =
  [ (x-1, y-1), (x-1, y), (x-1, y+1)
  , (x,   y-1),           (x,   y+1)
  , (x+1, y-1), (x+1, y), (x+1, y+1)
  ]

neighboursLvl3 (x, y) =
  [ (x-2, y-2), (x-2, y-1), (x-2, y), (x-2, y+1), (x-2, y+2)
  , (x-1, y-2),                                   (x-1, y+2)
  , (x,   y-2),                                   (x,   y+2)
  , (x+1, y-2),                                   (x+1, y+2)
  , (x+2, y-2), (x+2, y-1), (x+2, y), (x+2, y+1), (x+2, y+2)
  ]
