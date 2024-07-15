module Minefield.Game.Types where

import CPrelude

import qualified Data.Map as Map

type Pos = (Int, Int)
type PlayerPos = Pos
type ActorPos  = Pos

data ActorEvent
  = AddOverhaulIcon Char
  | SetEnabled Bool
  deriving (Show, Eq, Ord)

data SystemEvent
  = PlayerInputInvitedEvent
  | PlayerInputEvent PlayerPos !Text
  | PopulateCellDescriptionEvent
  | FieldIconEvent Pos Char

  | ActorEvent ActorPos ActorEvent
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

type GameIO a = IO a

type GameAction = SystemBus -> Pos -> GameIO ()

data Direction = U | D | L | R
  deriving (Show, Eq, Ord)
