{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import CPrelude

import Minefield.Core.Interface
import Minefield.Core.Game
import Minefield.Game.Types
import Minefield.Game.Game

import Minefield.Extensions.Nouns.Player
import Minefield.Extensions.Nouns.EmptyCell
import Minefield.Extensions.Nouns.Landmine
import Minefield.Extensions.Nouns.TimerBomb
import Minefield.Extensions.Verbs.Dig
import Minefield.Extensions.Verbs.PutFlag
import Minefield.Extensions.Verbs.UseLandmineDetector

import Minefield.Extensions.Materialization
import Minefield.Extensions.Implementation
import Extra.Implementation

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

type MyGame = GameDef
  Minefield
  (Player "@")
  (EmptyCell " ")
  '[ Landmine "A" 1
   , Landmine "B" 2
   , Landmine "C" 3
   , TimerBomb "6" 6
   , TimerBomb "7" 7
   , TimerBomb "8" 8
   ]
  '[ PutFlag
  --  , Dig
  --  , UseLandmineDetector
   ]


-- N.B., field sizes other than (7,7) are not properly supported
main :: IO ()
main = do
  -- game <- createRandomGame @MyGame 0.8 (7, 7)
  game <- createGame @MyGame

  grGameOrchestrator game
