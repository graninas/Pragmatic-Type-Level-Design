{-# LANGUAGE AllowAmbiguousTypes #-}

module Minefield.Core.Game where

import CPrelude

import Minefield.Core.Language
import Minefield.Core.System
import Minefield.Core.Eval

import GHC.TypeLits
import qualified Data.List as L
import qualified Data.Map as Map
import System.Random (randomRIO)


data TickVar = TickVar
  { tvInVar :: MVar ()
  , tvOutVar :: MVar ()
  }
type EndGameVar = MVar ()

data Actor = Actor
  { aThreadId :: ThreadId
  , aTickVar  :: TickVar
  }

type Field = Map.Map (Int, Int) Actor
type Actors = [((Int, Int), Actor)]

data GameRuntime = GameRuntime
  { grFieldRef :: IORef Field
  , grFieldWatcher :: ThreadId
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

  let getIcon  = Proxy @GetIcon
  let pIcon    = eval getIcon $ Proxy @player
  let ecIcon   = eval getIcon $ Proxy @emptyCell
  let objIcons = eval getIcon $ Proxy @(Objects objects)

  let coords = [(x, y) | x <- [1..w], y <- [1..h]]

  cells1 <- mapM (createRandomCell objIcons) coords
  cells2 <- writeEmptyCells ecIcon emptyCellsPercent cells1
  cells3 <- writePlayer (w, h) pIcon cells2

  -- print $ Map.toList cells2

  sysBus <- createSystemBus

  -- Creating actors for each cell
  actors <- mapM (createActor sysBus) $ Map.toList cells3
  let field = Map.fromList actors
  fieldRef <- newIORef field

  fieldWatcherId <- createFieldWatcher sysBus

  pure $ GameRuntime
    fieldRef
    fieldWatcherId
    (runGameOrchestrator actors sysBus)


data GamePhase
  = FieldDescription
  | PlayerInput
  | PlayerInputAwaiting

runGameOrchestrator
  :: Actors
  -> SystemBus
  -> IO ()
runGameOrchestrator actors sysBus = do
  print "Starting game orchestrator..."
  gameOrchestratorWorker PlayerInput

  where

    gameOrchestratorWorker PlayerInput = do
      publishSystemEvent sysBus PlayerInputInvitedEvent
      gameOrchestratorWorker PlayerInputAwaiting

    gameOrchestratorWorker PlayerInputAwaiting = do
      print "Ticking actors..."
      tickActors actors

      print "Delaying..."
      threadDelay $ 1000 * 100

      print "Reading events..."
      evs <- readEvents sysBus
      print $ "Events : " <> show evs

      dropEvents sysBus

      print "Dispatching events..."

      let inputEvs = [ev | ev <- evs, isPlayerInputEvent ev]
      case inputEvs of
        (PlayerInputEvent "quit" : _) ->
          print "Bye-bye"
        (PlayerInputEvent line : _) -> do
          print $ "Player line: " <> line
        _ -> do
          print "No player input yet"
          gameOrchestratorWorker PlayerInputAwaiting


createFieldWatcher
  :: SystemBus
  -> IO ThreadId
createFieldWatcher sysBus = do
  print "Creating field watcher..."
  inVar  <- newEmptyMVar
  outVar <- newEmptyMVar
  let tickVar = TickVar inVar outVar

  tId <- forkIO $ fieldWatcherWorker tickVar
  print "Field watcher created."
  pure tId

  where
    fieldWatcherWorker tickVar = do


      pure ()


createActor
  :: SystemBus
  -> ((Int, Int), Char)
  -> IO ((Int, Int), Actor)
createActor sysBus (p, ch) = do
  inVar  <- newEmptyMVar
  outVar <- newEmptyMVar
  let tickVar = TickVar inVar outVar

  -- Hardcode
  threadId <- case ch of
    '@' -> forkIO (playerWorker tickVar p ch)
    _   -> forkIO (actorWorker tickVar p ch)

  pure (p, Actor threadId tickVar)
  where
    playerWorker
      :: TickVar
      -> (Int, Int)
      -> Char
      -> IO ()
    playerWorker tickVar p ch = do

      waitForTick tickVar

      evs <- readEvents sysBus

      let inputEvs = [ev | ev <- evs, isPlayerInputInvitedEvent ev]
      case inputEvs of
        (_:_) -> do
          print "Type your command: "
          line <- getLine
          publishSystemEvent sysBus $ PlayerInputEvent line
        _ -> pure ()

      reportTickFinished tickVar

      playerWorker tickVar p ch

    actorWorker
      :: TickVar
      -> (Int, Int)
      -> Char
      -> IO ()
    actorWorker tickVar p ch = do

      waitForTick tickVar

      -- do stuff

      reportTickFinished tickVar

      actorWorker tickVar p ch


tickActors :: Actors -> IO ()
tickActors actors = mapM_ doTick actors
  where
    doTick (_, Actor _ tickVar) = do
      sendTick tickVar
      waitForFinishedTick tickVar

waitForTick (TickVar inVar _) = takeMVar inVar
reportTickFinished (TickVar _ outVar) = putMVar outVar ()
sendTick (TickVar inVar _) = putMVar inVar ()
waitForFinishedTick (TickVar _ outVar) = takeMVar outVar


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
