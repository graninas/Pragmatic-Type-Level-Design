{-# LANGUAGE AllowAmbiguousTypes #-}

module Minefield.Core.Game where

import CPrelude

import Minefield.Core.Language
import Minefield.Core.System
import Minefield.Core.Eval
import Minefield.Core.UI

import GHC.TypeLits
import qualified Data.List as L
import qualified Data.Map as Map
import System.Random (randomRIO)
import System.Console.ANSI


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

createRandomGame
  :: forall g field player emptyCell objects actions
   . ( g ~ Game field player emptyCell objects actions
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

  let coords = [(x, y) | x <- [1..w], y <- [1..h]]

  cells1 <- mapM (createRandomCell objIcons) coords
  cells2 <- writeEmptyCells ecIcon emptyCellsPercent cells1
  cells3 <- writePlayer (w, h) pIcon cells2

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
    (runGameOrchestrator orchQueueVar actors sysBus)


data GamePhase
  = RefreshUI
  | PlayerInput

-- | Game orchestrator. Manages events and provides a game loop.
runGameOrchestrator
  :: EventQueueVar
  -> Actors
  -> SystemBus
  -> IO ()
runGameOrchestrator queueVar actors sysBus = do
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
      -- TODO: proces events properly
      let inputEvs = [ev | ev <- evs, isPlayerInputEvent ev]
      case inputEvs of
        (PlayerInputEvent "quit" : _) -> print "Bye-bye"
        (PlayerInputEvent "exit" : _) -> print "Bye-bye"
        (PlayerInputEvent line : _) -> do
          -- print $ "Player line: " <> line
          gameOrchestratorWorker RefreshUI
        _ -> do
          -- print "No player input yet"
          gameOrchestratorWorker RefreshUI


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
      tId <- forkIO (playerWorker tickChan queueVar p ch)
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
    playerWorker
      :: TickChannel
      -> EventQueueVar
      -> (Int, Int)
      -> Char
      -> IO ()
    playerWorker tickChan queueVar p ch = do

      waitForTick tickChan

      evs <- takeEvents queueVar
      mapM_ (processPlayerEvent sysBus p ch) evs

      reportTickFinished tickChan

      playerWorker tickChan queueVar p ch

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

waitForTick (Channel inVar _) = takeMVar inVar
reportTickFinished (Channel _ outVar) = putMVar outVar ()
sendTick (Channel inVar _) = putMVar inVar ()
waitForFinishedTick (Channel _ outVar) = takeMVar outVar


createRandomCell
  :: [Char]
  -> (Int, Int)
  -> IO ((Int, Int), Char)
createRandomCell icons pos = do
  rndIdx <- randomRIO (0, (length icons) - 1)
  pure (pos, icons !! rndIdx)

writeEmptyCells
  :: Char
  -> Float
  -> [((Int, Int), Char)]
  -> IO (Map.Map (Int, Int) Char)
writeEmptyCells ecIcon percent cells = do

  let maxIdx = (length cells) - 1
  let cnt = truncate (percent * fromIntegral maxIdx)

  -- FIXME: not very reliable operation
  rndIndeces <- (L.take cnt . L.nub)
    <$> (replicateM (cnt * 3) $ randomRIO (0, maxIdx))

  let rndCells = map (\idx -> cells !! idx) rndIndeces

  let newMap1 = foldr (\(p, _) -> Map.insert p ecIcon) Map.empty rndCells
  let newMap2 = foldr maybeInsert newMap1 cells

  pure newMap2

  where
    maybeInsert (p, ch) field =
      Map.insertWith (\_ old -> old) p ch field

writePlayer
  :: (Int, Int)
  -> Char
  -> Map.Map (Int, Int) Char
  -> IO (Map.Map (Int, Int) Char)
writePlayer (w, h) ch cells = do
  x <- randomRIO (0, w - 1)
  y <- randomRIO (0, h - 1)
  pure $ Map.insert (x, y) ch cells


processPlayerEvent sysBus p ch PopulateCellDescriptionEvent =
  publishEvent sysBus $ FieldEvent p ch
processPlayerEvent sysBus p ch PlayerInputInvitedEvent = do
  line <- withInputInvitation "Type your command:"
  publishEvent sysBus $ PlayerInputEvent line
processPlayerEvent _ _ _ _ = pure ()

processActorEvent sysBus p ch PopulateCellDescriptionEvent =
  publishEvent sysBus $ FieldEvent p ch
processActorEvent _ _ _ _ = pure ()

processFieldWatcherEvent sysBus (w, h) (FieldEvent pos ch) =
  drawFieldObject pos ch
processFieldWatcherEvent _ _ _ = pure ()
