module Minefield.Core.Defaults where

import CPrelude

import Minefield.Core.Types


-- | Ticks in turn (list)
ticksInTurnList :: [Int]
ticksInTurnList = [0..9]

-- | Ticks in turn
ticksInTurn :: Int
ticksInTurn = length ticksInTurnList

-- | Special position for objects not presented on the field
nowherePos :: Pos
nowherePos = (-1, -1)

-- | Special number indicating the absence of object id
noObjectId :: ObjectId
noObjectId = 0

disarmedIcon :: Icon
disarmedIcon = '!'
