{-# LANGUAGE AllowAmbiguousTypes #-}

module Minefield.App.Game where

import CPrelude

import TypeLevelDSL.Eval
import Minefield.Core.Types
import Minefield.Core.System.Types
import Minefield.Core.System.Event
import Minefield.Core.System.Actor
import Minefield.Core.Interface
import Minefield.Core.Game
import Minefield.Core.Object
import Minefield.Core.Defaults

import Minefield.App.Runtime
import Minefield.App.RndGen
import Minefield.App.Player
import Minefield.App.UI

import Minefield.Implementation.Materialization
import Minefield.Implementation.SystemNouns.Player
import Minefield.Implementation.SystemNouns.EmptyCell

import GHC.TypeLits
import qualified Data.List as L
import qualified Data.Map as Map
import System.Random (randomRIO)
import System.Console.ANSI


tickThreadDelay :: Int
tickThreadDelay = 1000 * 50

-- | Creates a game from a definition.
createGameApp
  :: forall g field player emptyCell objects actions
   . ( g ~ GameDef field player emptyCell objects actions
     , EvalIO () MakeGameActions (ActsObjs actions objects) GameActions

     , EvalIO (SystemBus, FieldObjects)
          MakeActors
          (Objects (player ': emptyCell ': objects))
          [Actor]

     , EvalIO () GetObjectInfo player ObjectInfo
     , EvalIO () GetObjectInfo emptyCell ObjectInfo
     , EvalIO () GetObjectInfo (Objects objects) [ObjectInfo]
     , EvalIO (Int, Map Icon ObjectInfo) MaterializeField field FieldObjects
     )
  => IO AppRuntime
createGameApp = do
  resetScreen
  printTitle "Minefield game"

  sysBus <- createSystemBus

  -- Creating a random game

  pInfo  <- evalIO () GetObjectInfo $ Proxy @player
  ecInfo <- evalIO () GetObjectInfo $ Proxy @emptyCell
  objs   <- evalIO () GetObjectInfo $ Proxy @(Objects objects)

  let objs' = pInfo : ecInfo : objs
  let objInfosMap = Map.fromList $ map (\o -> (fst $ oiIcons o, o)) objs'

  field <- evalIO (0 :: Int, objInfosMap) MaterializeField
      $ Proxy @field
  let ((x, y), _) = Map.findMax field
  let (w, h) = (x + 1, y + 1)

  -- Subscribing the orchestrator
  let orchCond ev = isPlayerInputEvent ev
  orchQueueVar <- createQueueVar
  let orchSub = Subscription orchCond orchQueueVar
  subscribeRecipient sysBus orchSub

  -- Creating actors for each cell
  fieldActors <- evalIO (sysBus, field) MakeActors
      $ Proxy @(Objects (player ': emptyCell ': objects))

  -- Creating a special actor - field watcher
  fieldWatcher <- createFieldWatcherActor (w, h) sysBus
  let actors = fieldWatcher : fieldActors

  -- Making actions
  actions <- evalIO () MakeGameActions
    $ Proxy @(ActsObjs actions objects)

  printActions actions

  turnRef <- newIORef 1
  tickRef <- newIORef $ head ticksInTurnList
  let gr = GameRuntime
            sysBus
            actors
            actions
            (w, h)
            turnRef
            tickRef

  let orchestrator = gameOrchestratorWorker orchQueueVar
  pure $ AppRuntime gr orchestrator

-- | Creates a random game of given field size.
createRandomGameApp
  :: forall g field player emptyCell objects actions
   . ( g ~ GameDef field player emptyCell objects actions
     , EvalIO () MakeGameActions (ActsObjs actions objects) GameActions

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
  -> IO AppRuntime
createRandomGameApp emptyCellsPercent (w, h) = do
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
  fieldActors <- evalIO (sysBus, cells3) MakeActors
      $ Proxy @(Objects (player ': emptyCell ': objects))

  -- Creating a special actor - field watcher
  fieldWatcher <- createFieldWatcherActor (w, h) sysBus
  let actors = fieldWatcher : fieldActors

  -- Making actions
  actions <- evalIO () MakeGameActions
    $ Proxy @(ActsObjs actions objects)

  printActions actions

  turnRef <- newIORef 1
  tickRef <- newIORef $ head ticksInTurnList
  let gr = GameRuntime
            sysBus
            actors
            actions
            (w, h)
            turnRef
            tickRef

  let orchestrator = gameOrchestratorWorker orchQueueVar
  pure $ AppRuntime gr orchestrator

-- | Game orchestrator. Manages events and provides a game loop.
gameOrchestratorWorker
  :: EventQueueVar
  -> GameRuntime
  -> GamePhase
  -> GameIO ()
gameOrchestratorWorker queueVar gr phase = do
  goWorker phase

  where
    goWorker Start = do
      refreshUI False gr
      goWorker PlayerInput

    goWorker Idle = do
      refreshUI False gr
      goWorker PlayerInput

    goWorker DoTick = do
      turnFinished <- performTick gr
      refreshUI turnFinished gr
      goWorker PlayerInput

    goWorker DoTurn = do
      lastTick <- performTick gr
      refreshUI lastTick gr
      threadDelay tickThreadDelay

      if lastTick
        then goWorker PlayerInput
        else goWorker DoTurn

    goWorker PlayerInput = do
      let sysBus = grSysBus gr

      publishEvent sysBus PlayerInputInvitedEvent
      performActors gr
      distributeEvents sysBus     -- expecting special player input event

      evs <- extractEvents queueVar

      -- TODO: proper event processing
      let inputEvs = [ev | ev <- evs, isPlayerInputEvent ev]
      case inputEvs of
        (PlayerInputEvent _ "." : _)    -> goWorker DoTick
        (PlayerInputEvent _ "t" : _)    -> goWorker DoTurn
        (PlayerInputEvent _ "tick" : _) -> goWorker DoTick
        (PlayerInputEvent _ "turn" : _) -> goWorker DoTurn
        (PlayerInputEvent _ "quit" : _) -> printFarewell
        (PlayerInputEvent _ "exit" : _) -> printFarewell
        (PlayerInputEvent playerPos line : _) -> do

          eCmd <- parsePlayerCommand line $ grGameActions gr

          case eCmd of
            Left err -> do
              tick <- readIORef $ grTickRef gr
              turn <- readIORef $ grTurnRef gr
              printStatus turn tick err
              goWorker Idle
            Right playerCmd -> do
              performPlayerCommand sysBus playerPos playerCmd
              goWorker DoTurn

        _ -> goWorker Idle

createFieldWatcherActor
  :: (Int, Int)
  -> SystemBus
  -> GameIO Actor
createFieldWatcherActor (w, h) sysBus = do
  inVar  <- newEmptyMVar
  outVar <- newEmptyMVar
  let stepChan = Channel inVar outVar

  queueVar <- createQueueVar
  tId <- forkIO $ fieldWatcherWorker stepChan queueVar (w, h)

  let sub ev = isFieldIconEvent ev || isDebugMessageEvent ev
  subscribeRecipient sysBus $ Subscription sub queueVar

  pure $ SystemActor tId stepChan queueVar

  where
    fieldWatcherWorker stepChan queueVar (w, h) = do

      waitForStep stepChan

      clearField (w, h)
      drawFieldFrame (w, h)

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
processFieldWatcherEvent _ _ (DebugMessageEvent msg) =
  printDebugMessage msg
processFieldWatcherEvent _ _ _ = pure ()

printActions :: GameActions -> GameIO ()
printActions actions = do
  let cmds = map (\(a, (isDir, _)) -> (a, isDir)) $ Map.toList actions
  printCommands cmds

performActors :: GameRuntime -> GameIO ()
performActors gr = do
  distributeEvents $ grSysBus gr
  stepActors $ grActors gr

refreshUI :: Bool -> GameRuntime -> GameIO ()
refreshUI turnFinished gr = do
  let sysBus = grSysBus gr
  publishEvent sysBus PopulateIconEvent
  performActors gr

  tick <- readIORef $ grTickRef gr
  turn <- readIORef $ grTurnRef gr
  if turnFinished
    then printStatus turn tick "Turn finished."
    else printStatus turn tick ""

  flushScreen

performTick :: GameRuntime -> GameIO Bool
performTick gr = do
  tick <- readIORef $ grTickRef gr
  turn <- readIORef $ grTurnRef gr
  let sysBus = grSysBus gr
  let actors = grActors gr

  let newTick = tick + 1
  publishEvent sysBus $ TickEvent newTick

  if newTick == ticksInTurn
    then do
      let newTurn = turn + 1
      publishEvent sysBus $ TurnEvent turn

      writeIORef (grTurnRef gr) newTurn
      writeIORef (grTickRef gr) $ head ticksInTurnList
    else do
      writeIORef (grTickRef gr) newTick

  pure $ newTick == ticksInTurn


-- Ticks and turns

--  1    2    3
--  |    |    |
--  ===============
--  0123401234
--  ^^^^^
--       ^
--       end of turn 1 (Turn 1 will be sent)
--       ^^^^^
--            ^
--            end of turn 2
