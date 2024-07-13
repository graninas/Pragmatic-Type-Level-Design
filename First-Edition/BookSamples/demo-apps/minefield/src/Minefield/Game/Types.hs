module Minefield.Game.Types where

import CPrelude

import Minefield.Core.System

import qualified Data.Map as Map


data Channel inT outT = Channel
  { cInVar  :: MVar inT
  , cOutVar :: MVar outT
  }

type TickChannel = Channel () ()
type EndGameVar = MVar ()

data Actor = Actor
  { aThreadId :: ThreadId
  , aTickChannel :: TickChannel
  , aInEventQueueVar :: EventQueueVar
  }

type Field = Map.Map (Int, Int) Actor
type Actors = [((Int, Int), Actor)]

data GameRuntime = GameRuntime
  { grFieldRef :: IORef Field
  , grFieldSize :: (Int, Int)
  , grGameOrchestrator :: IO ()
  }

type EmptyCellsPercent = Float


data GamePhase
  = RefreshUI
  | PlayerInput


waitForTick :: TickChannel -> IO ()
waitForTick (Channel inVar _) = takeMVar inVar

reportTickFinished :: TickChannel -> IO ()
reportTickFinished (Channel _ outVar) = putMVar outVar ()

sendTick :: TickChannel -> IO ()
sendTick (Channel inVar _) = putMVar inVar ()

waitForFinishedTick :: TickChannel -> IO ()
waitForFinishedTick (Channel _ outVar) = takeMVar outVar
