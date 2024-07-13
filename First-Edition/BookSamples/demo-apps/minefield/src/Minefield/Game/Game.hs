{-# LANGUAGE AllowAmbiguousTypes #-}

module Minefield.Game.Game where

import CPrelude

import Minefield.Core.Language
import Minefield.Core.Eval

import Minefield.Game.Types
import Minefield.Game.RndGen
import Minefield.Game.Player
import Minefield.Game.System
import Minefield.Game.UI

import Minefield.Extensions.Materialization

import GHC.TypeLits
import qualified Data.List as L
import qualified Data.Map as Map
import System.Random (randomRIO)
import System.Console.ANSI


-- | Creates a random game of given field size.
createRandomGame
  :: forall g field player emptyCell objects actions
   . ( g ~ Game field player emptyCell objects actions
     , Eval MakeCommands (ObjsActs objects actions) PlayerCommands
     , Eval GetIcon player Char
     , Eval GetIcon emptyCell Char
     , Eval GetIcon (Objects objects) [Char]
     )
  => EmptyCellsPercent
  -> (Int, Int)
  -> IO GameRuntime
createRandomGame emptyCellsPercent (w, h) = do
  resetScreen
  printTitle "Minefield game"

  let getIcon  = Proxy @GetIcon
  let pIcon    = eval getIcon $ Proxy @player
  let ecIcon   = eval getIcon $ Proxy @emptyCell
  let objIcons = eval getIcon $ Proxy @(Objects objects)

  let makeCommands = Proxy @MakeCommands
  let cmds = eval makeCommands $ Proxy @(ObjsActs objects actions)

  let coords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]

  cells1 <- mapM (createRandomCell objIcons) coords
  cells2 <- writeRandomEmptyCells ecIcon emptyCellsPercent cells1
  cells3 <- writeRandomPlayer (w, h) pIcon cells2


  sysBus <- createSystemBus

  -- Subscribing the orchestrator
  let orchCond ev = isPlayerInputEvent ev
  orchQueueVar <- createQueueVar
  let orchSub = Subscription orchCond orchQueueVar
  subscribeRecipient sysBus orchSub

  fieldWatcher <- createFieldWatcherActor (w, h) sysBus
  -- TODO: subscribe the field watcher

  -- Creating actors for each cell
  fieldActors <- mapM (createActor sysBus) $ Map.toList cells3
  let field = Map.fromList fieldActors
  fieldRef <- newIORef field

  let actors = ((-1, -1), fieldWatcher) : fieldActors

  pure $ GameRuntime
    fieldRef
    (w, h)
    (runGameOrchestrator sysBus orchQueueVar actors cmds)


-- | Game orchestrator. Manages events and provides a game loop.
runGameOrchestrator
  :: SystemBus
  -> EventQueueVar
  -> Actors
  -> PlayerCommands
  -> IO ()
runGameOrchestrator sysBus queueVar actors cmds = do
  -- print "Starting game orchestrator..."
  gameOrchestratorWorker RefreshUI

  where

    gameOrchestratorWorker RefreshUI = do
      publishEvent sysBus PopulateCellDescriptionEvent
      distributeEvents sysBus

      -- print "RefreshUI: ticking actors..."

      tickActors actors

      -- print "TODO: RefreshUI: ticking field watcher..."

      gameOrchestratorWorker PlayerInput

    gameOrchestratorWorker PlayerInput = do
      publishEvent sysBus PlayerInputInvitedEvent
      distributeEvents sysBus

      -- print "Ticking actors..."
      tickActors actors

      distributeEvents sysBus

      -- print "Reading orchestrator's events..."
      evs <- takeEvents queueVar
      -- print $ "Events: " <> show evs

      -- print "Processing events..."
      let inputEvs = [ev | ev <- evs, isPlayerInputEvent ev]
      case inputEvs of
        (PlayerInputEvent "quit" : _) -> print "Bye-bye"
        (PlayerInputEvent "exit" : _) -> print "Bye-bye"
        (PlayerInputEvent line : _) -> do

          let mbCmd = parsePlayerCommand cmds line

          gameOrchestratorWorker RefreshUI
        _ -> gameOrchestratorWorker RefreshUI


createFieldWatcherActor
  :: (Int, Int)
  -> SystemBus
  -> IO Actor
createFieldWatcherActor (w, h) sysBus = do
  inVar  <- newEmptyMVar
  outVar <- newEmptyMVar
  let tickChan = Channel inVar outVar

  clearField (w, h)
  drawFieldFrame (w, h)

  queueVar <- createQueueVar
  tId <- forkIO $ fieldWatcherWorker tickChan queueVar (w, h)

  let sub = isFieldEvent
  subscribeRecipient sysBus $ Subscription sub queueVar

  pure $ Actor tId tickChan queueVar

  where
    fieldWatcherWorker tickChan queueVar (w, h) = do

      waitForTick tickChan

      evs <- takeEvents queueVar
      mapM_ (processFieldWatcherEvent sysBus (w, h)) evs

      reportTickFinished tickChan

      fieldWatcherWorker tickChan queueVar (w, h)


createActor
  :: SystemBus
  -> ((Int, Int), Char)
  -> IO ((Int, Int), Actor)
createActor sysBus (p, ch) = do
  inVar  <- newEmptyMVar
  outVar <- newEmptyMVar
  let tickChan = Channel inVar outVar

  queueVar <- createQueueVar

  -- TODO: FIXME: Hardcode
  (threadId, subCond) <- case ch of
    '@' -> do
      tId <- forkIO (playerWorker sysBus tickChan queueVar p ch)
      let sub ev =
            isPlayerInputInvitedEvent ev
            || isPopulateCellDescriptionEvent ev
      pure (tId, sub)

    _   -> do
      tId <- forkIO (actorWorker tickChan queueVar p ch)
      let sub = isPopulateCellDescriptionEvent
      pure (tId, sub)

  subscribeRecipient sysBus $ Subscription subCond queueVar

  pure (p, Actor threadId tickChan queueVar)
  where
    actorWorker
      :: TickChannel
      -> EventQueueVar
      -> (Int, Int)
      -> Char
      -> IO ()
    actorWorker tickChan queueVar p ch = do

      waitForTick tickChan

      evs <- takeEvents queueVar
      mapM_ (processActorEvent sysBus p ch) evs

      reportTickFinished tickChan

      actorWorker tickChan queueVar p ch


tickActors :: Actors -> IO ()
tickActors actors = mapM_ doTick actors
  where
    doTick (_, Actor _ tickChan _) = do
      sendTick tickChan
      waitForFinishedTick tickChan

processActorEvent sysBus p ch PopulateCellDescriptionEvent =
  publishEvent sysBus $ FieldEvent p ch
processActorEvent _ _ _ _ = pure ()

processFieldWatcherEvent sysBus (w, h) (FieldEvent pos ch) =
  drawFieldObject pos ch
processFieldWatcherEvent _ _ _ = pure ()
