module Minefield.Game.Types where

import CPrelude

import qualified Data.Map as Map


data SystemEvent
  = PlayerInputInvitedEvent
  | PlayerInputEvent !Text
  | PopulateCellDescriptionEvent
  | FieldEvent (Int, Int) Char
  deriving (Show, Eq, Ord)

type EventQueueVar = MVar [SystemEvent]

data SystemBus = SystemBus
  { sbEventsVar :: EventQueueVar
  , sbSubscriptionsVar :: MVar [Subscription]
  }

data Subscription = Subscription
  { sCondition :: SystemEvent -> Bool
  , sRecipientQueueVar :: EventQueueVar
  }

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

type PlayerCommands = Int
type PlayerCommand = Int

