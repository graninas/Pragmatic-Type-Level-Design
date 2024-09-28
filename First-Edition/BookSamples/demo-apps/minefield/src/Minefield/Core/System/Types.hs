module Minefield.Core.System.Types where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object

import qualified Data.Map as Map


data ActorRequest
  = AddOverhaulIcon OverhaulIcon
  | SetEnabled Bool
  | SetDisarmed Bool
  | SetExplosion

  | GenericRequest String
      -- ^ Any request that can be encoded as string
  deriving (Show, Eq, Ord)

data SystemEvent
  = PlayerInputRequestEvent
  | PlayerInputEvent PlayerPos String
  | PopulateIconRequestEvent
  | IconEvent Pos Icon

  | TickEvent Int   -- Current tick
  | TurnEvent Int   -- Current turn

  | DebugMessageEvent String

  | ActorRequestEvent ObjectType ActorPos ActorRequest
                      -- ^ Empty ObjectType means "any match"
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

type GameIO a = IO a

type TextCommand = String

data StepChannel = StepChannel
  { cInVar  :: MVar ()
  , cOutVar :: MVar ()
  }

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


