module Minefield.Game.Types where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object

import qualified Data.Map as Map

type PlayerPos = Pos
type ActorPos  = Pos

data ObjectRequestEvent
  = AddOverhaulIcon OverhaulIcon
  | SetEnabled Bool
  deriving (Show, Eq, Ord)

data SystemEvent
  = PlayerInputInvitedEvent
  | PlayerInputEvent PlayerPos String
  | PopulateCellDescriptionEvent
  | FieldIconEvent Pos Icon
  | TickEvent
  | TurnEvent

  | ObjectRequestEvent ObjectType ActorPos ObjectRequestEvent
  deriving (Show, Eq, Ord)

type EventQueueVar = MVar [SystemEvent]

data Subscription = Subscription
  { sCondition :: SystemEvent -> Bool
  , sRecipientQueueVar :: EventQueueVar
  }

data SystemBus = SystemBus
  { sbEventsVar :: EventQueueVar
  , sbSubscriptionsVar :: MVar [Subscription]
  }

data Channel inT outT = Channel
  { cInVar  :: MVar inT
  , cOutVar :: MVar outT
  }

type StepChannel = Channel () ()
type EndGameVar = MVar ()

data Actor
  = Actor
    { aThreadId :: ThreadId
    , aStepChannel :: StepChannel
    , aInEventQueueVar :: EventQueueVar
    }
  | SystemActor
    { aThreadId :: ThreadId
    , aStepChannel :: StepChannel
    , aInEventQueueVar :: EventQueueVar
    }

type Field = Map.Map (Int, Int) Actor
type Actors = [Actor]

data GameRuntime = GameRuntime
  { grFieldSize :: (Int, Int)
  , grGameOrchestrator :: IO ()
  }

type EmptyCellsPercent = Float

data GamePhase
  = RefreshUI
  | PlayerInput
  | DoTurn

type GameIO a = IO a

type TextCommand = String

type ActorAction  = SystemBus -> Pos -> GameIO ()
type ActorActions = Map.Map ObjectType ActorAction
type GameActions  = Map.Map TextCommand (Bool, ActorActions)

data Direction = U | D | L | R
  deriving (Show, Eq, Ord, Read)

data PlayerCommand
  = PlayerCommand (Maybe Direction) ActorActions


type FieldObjects = Map.Map (Int, Int) ObjectType
