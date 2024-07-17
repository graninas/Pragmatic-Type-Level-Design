{-# LANGUAGE AllowAmbiguousTypes #-}

module Minefield.Game.Game where

import CPrelude

import TypeLevelDSL.Eval
import Minefield.Core.Types
import Minefield.Core.Interface
import Minefield.Core.Object

import Minefield.Game.Types
import Minefield.Game.RndGen
import Minefield.Game.Player
import Minefield.Game.System
import Minefield.Game.UI

import Minefield.Extensions.Materialization
import Minefield.Extensions.Implementation

import GHC.TypeLits
import qualified Data.List as L
import qualified Data.Map as Map
import System.Random (randomRIO)
import System.Console.ANSI


ticksInTurn :: Int
ticksInTurn = 10


data GamePhase
  = RefreshUI
  | PlayerInput
  | DoTurn

type TicksLeft = Int
type GameTurn = (GamePhase, TicksLeft)

-- | Creates a random game of given field size.
createRandomGame
  :: forall g field player emptyCell objects actions
   . ( g ~ Game field player emptyCell objects actions
     , EvalIO () MakeGameActions (ObjsActs objects actions) GameActions

     , EvalIO (SystemBus, FieldObjects)
          MakeActors
          (Objects (player ': emptyCell ': objects))
          [Actor]

     , EvalIO () GetObjectInfo player ObjectInfo
     , EvalIO () GetObjectInfo emptyCell ObjectInfo
     , EvalIO () GetObjectInfo (Objects objects) [ObjectInfo]
     )
  => EmptyCellsPercent
  -> (Int, Int)
  -> IO GameRuntime
createRandomGame emptyCellsPercent (w, h) = do
  resetScreen
  printTitle "Minefield game"

  sysBus <- createSystemBus

  -- Creating a random game

  pInfo  <- evalIO () GetObjectInfo $ Proxy @player
  ecInfo <- evalIO () GetObjectInfo $ Proxy @emptyCell
  objs   <- evalIO () GetObjectInfo $ Proxy @(Objects objects)

  let coords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
  cells1 <- mapM (createRandomCell objs) coords
  cells2 <- writeRandomEmptyCells ecInfo emptyCellsPercent cells1
  cells3 <- writeRandomPlayer (w, h) pInfo cells2

  -- Subscribing the orchestrator
  let orchCond ev = isPlayerInputEvent ev
  orchQueueVar <- createQueueVar
  let orchSub = Subscription orchCond orchQueueVar
  subscribeRecipient sysBus orchSub

  -- Creating actors for each cell
  let fieldObjects = Proxy @(Objects (player ': emptyCell ': objects))
  fieldActors <- evalIO (sysBus, cells3) MakeActors fieldObjects

  -- Creating a special actor - field watcher
  fieldWatcher <- createFieldWatcherActor (w, h) sysBus
  let actors = fieldWatcher : fieldActors

  -- Making actions
  actions <- evalIO () MakeGameActions
    $ Proxy @(ObjsActs objects actions)

  pure $ GameRuntime
    (w, h)
    (runGameOrchestrator sysBus orchQueueVar actors actions)


-- | Game orchestrator. Manages events and provides a game loop.
runGameOrchestrator
  :: SystemBus
  -> EventQueueVar
  -> Actors
  -> GameActions
  -> GameIO ()
runGameOrchestrator sysBus queueVar actors actions = do
  gameOrchestratorWorker (RefreshUI, ticksInTurn)

  where

    gameOrchestratorWorker (RefreshUI, ticksLeft) = do
      publishEvent sysBus PopulateCellDescriptionEvent
      distributeEvents sysBus
      stepActors actors

      gameOrchestratorWorker (PlayerInput, ticksLeft)

    gameOrchestratorWorker (DoTurn, ticksLeft) = do

      mapM_ (\n -> do
        printStatus $ "Performing a tick: " <> show n
        doTick sysBus actors
        ) [1..ticksLeft]

      publishEvent sysBus TurnEvent
      distributeEvents sysBus
      stepActors actors

      gameOrchestratorWorker (RefreshUI, ticksInTurn)

    gameOrchestratorWorker (PlayerInput, ticksLeft) = do
      publishEvent sysBus PlayerInputInvitedEvent
      distributeEvents sysBus

      stepActors actors
      distributeEvents sysBus

      evs <- extractEvents queueVar

      -- TODO: proper event processing
      let inputEvs = [ev | ev <- evs, isPlayerInputEvent ev]
      case inputEvs of
        (PlayerInputEvent _ "tick" : _) -> case ticksLeft of
          0 -> gameOrchestratorWorker (DoTurn, ticksInTurn)
          _ -> do
            doTick sysBus actors
            gameOrchestratorWorker (PlayerInput, ticksLeft - 1)
        (PlayerInputEvent _ "turn" : _) -> gameOrchestratorWorker (DoTurn, ticksInTurn)
        (PlayerInputEvent _ "quit" : _) -> printStatus "Bye-bye"
        (PlayerInputEvent _ "exit" : _) -> printStatus "Bye-bye"
        (PlayerInputEvent playerPos line : _) -> do

          eCmd <- parsePlayerCommand line actions

          case eCmd of
            Left err -> do
              printStatus err
              gameOrchestratorWorker (RefreshUI, ticksLeft)
            Right playerCmd -> do
              performPlayerCommand sysBus playerPos playerCmd
              gameOrchestratorWorker (DoTurn, ticksInTurn)

        _ -> do

          gameOrchestratorWorker (RefreshUI, ticksLeft)

doTick :: SystemBus -> Actors -> GameIO ()
doTick sysBus actors = do
  publishEvent sysBus TickEvent
  distributeEvents sysBus
  stepActors actors

  publishEvent sysBus PopulateCellDescriptionEvent
  distributeEvents sysBus
  stepActors actors

  flushScreen

  threadDelay $ 1000 * 50


createFieldWatcherActor
  :: (Int, Int)
  -> SystemBus
  -> GameIO Actor
createFieldWatcherActor (w, h) sysBus = do
  inVar  <- newEmptyMVar
  outVar <- newEmptyMVar
  let stepChan = Channel inVar outVar

  clearField (w, h)
  drawFieldFrame (w, h)

  queueVar <- createQueueVar
  tId <- forkIO $ fieldWatcherWorker stepChan queueVar (w, h)

  let sub = isFieldIconEvent
  subscribeRecipient sysBus $ Subscription sub queueVar

  pure $ SystemActor tId stepChan queueVar

  where
    fieldWatcherWorker stepChan queueVar (w, h) = do

      waitForStep stepChan

      evs <- extractEvents queueVar
      mapM_ (processFieldWatcherEvent sysBus (w, h)) evs

      reportStepFinished stepChan

      fieldWatcherWorker stepChan queueVar (w, h)


stepActors :: Actors -> GameIO ()
stepActors actors = mapM_ doStep actors
  where
    doStep (Actor _ stepChan _) = do
      sendStep stepChan
      waitForFinishedStep stepChan
    doStep (SystemActor _ stepChan _) = do
      sendStep stepChan
      waitForFinishedStep stepChan

processFieldWatcherEvent
  :: SystemBus
  -> Pos
  -> SystemEvent
  -> GameIO ()
processFieldWatcherEvent sysBus (w, h) (FieldIconEvent pos ch) =
  drawFieldObject pos ch
processFieldWatcherEvent _ _ _ = pure ()
