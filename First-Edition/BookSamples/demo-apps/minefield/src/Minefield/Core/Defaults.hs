module Minefield.Core.Defaults where

import CPrelude


-- | Ticks in turn (list)
ticksInTurnList :: [Int]
ticksInTurnList = [0..9]

-- | Ticks in turn
ticksInTurn :: Int
ticksInTurn = length ticksInTurnList
