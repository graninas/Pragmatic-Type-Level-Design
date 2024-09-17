module Minefield.Game.Types where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object

import qualified Data.Map as Map


-- TODO: refactor this mess


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
  , grTicksInTurn :: Int
  , grTurnRef :: IORef Int
  , grTickRef :: IORef Int
  }

data AppRuntime = AppRuntime
  { arGameRuntime :: GameRuntime
  , arGameOrchestratorWorker :: GameRuntime -> GamePhase -> IO ()
  }

type EmptyCellsPercent = Float

type GameIO a = IO a

type TextCommand = String
type IsDirected = Bool

type ActorAction  = SystemBus -> ObjectType -> Pos -> GameIO ()
type ActorActions = Map.Map ObjectType ActorAction
type GameActions  = Map.Map TextCommand (IsDirected, ActorActions)

data Direction
  = U | D | L | R
  | UL | UR | DL | DR
  deriving (Show, Eq, Ord, Read)

data PlayerCommand
  = PlayerCommand (Maybe Direction) ActorActions


type FieldObjects = Map.Map (Int, Int) ObjectInfo
