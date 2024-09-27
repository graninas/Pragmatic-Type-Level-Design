{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import CPrelude

import Minefield.Core.Interface
import Minefield.Core.Game
import Minefield.Core.Types
import Minefield.Core.System.Types
import Minefield.App.Game
import Minefield.App.Runtime

import Minefield.Implementation.Materialization

import Minefield.Implementation.SystemNouns.Player
import Minefield.Implementation.SystemNouns.EmptyCell
import Minefield.Implementation.SystemVerbs.PutFlag

import Minefield.Extensions.Nouns.Landmine
import Minefield.Extensions.Nouns.TimerBomb
import Minefield.Extensions.Verbs.Dig
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
   ]


type SimpleMinefield =
  '[ "           6@"
   , "            A"
   , "             "
   , "            X"
   , "             "
   , "             "
   , "             "
   ]

type SimpleGame = GameDef
  SimpleMinefield
  (Player "@")
  (EmptyCell " ")
  '[ TimerBomb "6" 6
   , Landmine "A" 1
   ]
  '[ PutFlag
   ]

-- N.B., field sizes other than (7,7) are not properly supported
main :: IO ()
main = do
  -- appRt <- createRandomGameApp @MyGame 0.8 (7, 7)
  appRt <- createGameApp @SimpleGame

  let gameRt = arGameRuntime appRt
  let orchestrator = arGameOrchestratorWorker appRt
  orchestrator gameRt Start
