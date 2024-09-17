{-# LANGUAGE AllowAmbiguousTypes #-}

module Minefield.Game.Game where

import CPrelude

import TypeLevelDSL.Eval
import Minefield.Core.Types
import Minefield.Core.Interface
import Minefield.Core.Game
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


defaultTicksInTurn :: Int
defaultTicksInTurn = 10

tickThreadDelay :: Int
tickThreadDelay = 1000 * 50


-- | Creates a game from a definition.
createGameApp
  :: forall g field player emptyCell objects actions
   . ( g ~ GameDef field player emptyCell objects actions
     , EvalIO () MakeGameActions (ObjsActs objects actions) GameActions

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
  let objInfosMap = Map.fromList $ map (\o -> (oiIcon o, o)) objs'

  field <- evalIO ( 0 :: Int, objInfosMap) MaterializeField
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
    $ Proxy @(ObjsActs objects actions)

  printActions actions

  turnRef <- newIORef 1
  tickRef <- newIORef 1
  let gr = GameRuntime
            sysBus
            actors
            actions
            (w, h)
            defaultTicksInTurn
            turnRef
            tickRef

  let orchestrator = gameOrchestratorWorker orchQueueVar
  pure $ AppRuntime gr orchestrator

-- | Creates a random game of given field size.
createRandomGameApp
  :: forall g field player emptyCell objects actions
   . ( g ~ GameDef field player emptyCell objects actions
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
    $ Proxy @(ObjsActs objects actions)

  printActions actions

  turnRef <- newIORef 1
  tickRef <- newIORef 1
  let gr = GameRuntime
            sysBus
            actors
            actions
            (w, h)
            defaultTicksInTurn
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
      publishEvent (grSysBus gr) TurnEvent
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
            Left err -> goWorker Idle
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
  publishEvent sysBus PopulateCellDescriptionEvent
  performActors gr

  tick <- readIORef $ grTickRef gr
  turn <- readIORef $ grTurnRef gr
  let ticksInTurn = grTicksInTurn gr
  if turnFinished
    then printStatus turn tick ticksInTurn "Turn finished."
    else printStatus turn tick ticksInTurn ""

  flushScreen

performTick :: GameRuntime -> GameIO Bool
performTick gr = do
  -- N.B. Tick should be correct
  tick <- readIORef $ grTickRef gr
  turn <- readIORef $ grTurnRef gr
  let ticksInTurn = grTicksInTurn gr
  let sysBus = grSysBus gr
  let actors = grActors gr

  publishEvent sysBus TickEvent

  let newTick = tick + 1
  if newTick >= ticksInTurn
    then do
      let newTurn = turn + 1
      publishEvent sysBus TurnEvent

      writeIORef (grTurnRef gr) newTurn
      writeIORef (grTickRef gr) 1
    else do
      writeIORef (grTickRef gr) newTick

  pure $ newTick >= ticksInTurn



--  1    2    3
--  |    |    |
--  ===============
--  1234512345
--  ^^^^^
--       ^
--       ^^^^^
--            ^
--
--  1
--  1

-- do turn:
-- last ticks
--    (does turn as well)

-- do tick:
-- tick
-- if last tick
--    then turn
--    else ()
