{-# LANGUAGE AllowAmbiguousTypes #-}

module Minefield.Game.Game where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Types
import Minefield.Core.Interface

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


-- | Creates a random game of given field size.
createRandomGame
  :: forall g field player emptyCell objects actions
   . ( g ~ Game field player emptyCell objects actions
     , Eval () MakeGameActions (ObjsActs objects actions) GameActions

     , Eval (SystemBus, FieldObjects)
          MakeActors
          (Objects (player ': emptyCell ': objects))
          [Actor]

     , Eval () GetObjectType player ObjectType
     , Eval () GetObjectType emptyCell ObjectType
     , Eval () GetObjectType (Objects objects) [ObjectType]
     )
  => EmptyCellsPercent
  -> (Int, Int)
  -> IO GameRuntime
createRandomGame emptyCellsPercent (w, h) = do
  resetScreen
  printTitle "Minefield game"

  sysBus <- createSystemBus

  -- Creating a random game

  let getObjectType = Proxy @GetObjectType
  pType  <- eval () getObjectType $ Proxy @player
  ecType <- eval () getObjectType $ Proxy @emptyCell
  objs   <- eval () getObjectType $ Proxy @(Objects objects)

  let coords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
  cells1 <- mapM (createRandomCell objs) coords
  cells2 <- writeRandomEmptyCells ecType emptyCellsPercent cells1
  cells3 <- writeRandomPlayer (w, h) pType cells2

  -- Subscribing the orchestrator
  let orchCond ev = isPlayerInputEvent ev
  orchQueueVar <- createQueueVar
  let orchSub = Subscription orchCond orchQueueVar
  subscribeRecipient sysBus orchSub

  -- Creating actors for each cell
  let makeActors   = Proxy @MakeActors
  let fieldObjects = Proxy @(Objects (player ': emptyCell ': objects))
  fieldActors <- eval (sysBus, cells3) makeActors fieldObjects

  -- Creating a special actor - field watcher
  fieldWatcher <- createFieldWatcherActor (w, h) sysBus
  let actors = fieldWatcher : fieldActors

  -- Making actions
  let makeActions = Proxy @MakeGameActions
  actions <- eval () makeActions $ Proxy @(ObjsActs objects actions)

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
  gameOrchestratorWorker RefreshUI

  where

    gameOrchestratorWorker RefreshUI = do
      publishEvent sysBus PopulateCellDescriptionEvent
      distributeEvents sysBus
      stepActors actors

      gameOrchestratorWorker PlayerInput

    gameOrchestratorWorker DoTurn = do

      mapM_ (\(n :: Int) -> do
        printStatus $ "Performing a tick: " <> show n
        publishEvent sysBus TickEvent
        distributeEvents sysBus
        stepActors actors

        publishEvent sysBus PopulateCellDescriptionEvent
        distributeEvents sysBus
        stepActors actors

        flushScreen

        threadDelay $ 1000 * 10
        ) [1..9]

      publishEvent sysBus TurnEvent
      distributeEvents sysBus
      stepActors actors

      gameOrchestratorWorker RefreshUI

    gameOrchestratorWorker PlayerInput = do
      publishEvent sysBus PlayerInputInvitedEvent
      distributeEvents sysBus

      stepActors actors
      distributeEvents sysBus

      evs <- extractEvents queueVar

      -- TODO: proper event processing
      let inputEvs = [ev | ev <- evs, isPlayerInputEvent ev]
      case inputEvs of
        (PlayerInputEvent _ "turn" : _) -> gameOrchestratorWorker DoTurn
        (PlayerInputEvent _ "quit" : _) -> printStatus "Bye-bye"
        (PlayerInputEvent _ "exit" : _) -> printStatus "Bye-bye"
        (PlayerInputEvent playerPos line : _) -> do

          eCmd <- parsePlayerCommand line actions

          case eCmd of
            Left err -> do
              printStatus err
              gameOrchestratorWorker RefreshUI
            Right playerCmd -> do
              performPlayerCommand sysBus playerPos playerCmd
              gameOrchestratorWorker DoTurn

        _ -> do

          gameOrchestratorWorker RefreshUI


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
