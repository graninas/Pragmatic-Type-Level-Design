module Minefield.App.Runtime where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.System.Types
import Minefield.Core.Object


data GamePhase
  = Start
  | Idle
  | PlayerInput
  | DoTurn
  | DoTick

data GameRuntime = GameRuntime
  { grSysBus :: SystemBus
  , grActors :: Actors
  , grGameActions :: GameActions
  , grFieldSize :: (Int, Int)
  , grTurnRef :: IORef Int
  , grTickRef :: IORef Int
  }

data AppRuntime = AppRuntime
  { arGameRuntime :: GameRuntime
  , arGameOrchestratorWorker :: GameRuntime -> GamePhase -> IO ()
  }

type EmptyCellsPercent = Float

data PlayerCommand
  = PlayerCommand (Maybe Direction) ActorActions

