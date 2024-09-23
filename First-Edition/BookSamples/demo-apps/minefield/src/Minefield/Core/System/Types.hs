module Minefield.Core.System.Types where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object

import qualified Data.Map as Map


data ObjectRequest
  = AddOverhaulIcon OverhaulIcon
  | SetEnabled Bool
  | SetDisarmed Bool

  | GenericRequest String
      -- ^ Any request that can be encoded as string
  deriving (Show, Eq, Ord)

data SystemEvent
  = PlayerInputInvitedEvent
  | PlayerInputEvent PlayerPos String
  | PopulateIconEvent
  | FieldIconEvent Pos Icon

  | TickEvent Int   -- Current tick
  | TurnEvent Int   -- Current turn

  | DebugMessageEvent String

  | ObjectRequestEvent ObjectType ActorPos ObjectRequest
  deriving (Show, Eq, Ord)

data Subscription = Subscription
  { sCondition :: SystemEvent -> Bool
  , sRecipientQueueVar :: EventQueueVar
  }

type EventQueueVar = MVar [SystemEvent]

data SystemBus = SystemBus
  { sbEventsVar :: EventQueueVar
  , sbSubscriptionsVar :: MVar [Subscription]
  }

type GameIO a = IO a

type TextCommand = String

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

type Actors = [Actor]
type ActorAction  = SystemBus -> ObjectType -> Pos -> GameIO ()
type ActorActions = Map.Map ObjectType ActorAction
type GameActions  = Map.Map TextCommand (IsDirected, ActorActions)
type FieldObjects = Map.Map (Int, Int) ObjectInfo
type Field = Map.Map (Int, Int) Actor


