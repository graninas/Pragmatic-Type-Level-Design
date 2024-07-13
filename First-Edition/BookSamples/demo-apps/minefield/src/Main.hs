module Main where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Language
import Minefield.Game.Types
import Minefield.Game.Game

import Minefield.Extensions.Nouns.Player
import Minefield.Extensions.Nouns.EmptyCell
import Minefield.Extensions.Nouns.Landmine
import Minefield.Extensions.Nouns.TimerBomb
import Minefield.Extensions.Verbs.Dig
import Minefield.Extensions.Verbs.PutFlag
import Minefield.Extensions.Verbs.UseLandmineDetector

import GHC.TypeLits


-- Minefield
-- 6,7,8: timer bombs with 0.5 turns till detonation
-- A,B,C: landmines with power 1,2,3 respectively
-- X: player

type Minefield =
  '[ "7 6     A   @"
   , "  B 7 B     8"
   , "  C   7     B"
   , "  8     A 6  "
   , "B A   7 8   8"
   , "7     C   7 6"
   , "  A C     7 C"
   ]

type MyGame = Game
  Minefield         -- not used for now
  (Player "@")
  (EmptyCell " ")
  '[ Landmine "6" 6
   , Landmine "7" 7
   , Landmine "8" 8
   , TimerBomb "A" 16
   , TimerBomb "B" 18
   , TimerBomb "C" 20
   ]
  '[ PutFlag
   , Dig
   , UseLandmineDetector
   ]


main :: IO ()
main = do
  game <- createRandomGame @MyGame 0.8 (7, 7)

  grGameOrchestrator game
