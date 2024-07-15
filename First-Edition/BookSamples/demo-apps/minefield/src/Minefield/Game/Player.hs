module Minefield.Game.Player where

import CPrelude

import qualified Prelude as P

import Minefield.Game.Types
import Minefield.Game.System
import Minefield.Game.UI

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Random (randomRIO)


playerWorker
  :: SystemBus
  -> TickChannel
  -> EventQueueVar
  -> (Int, Int)
  -> Char
  -> GameIO ()
playerWorker sysBus tickChan queueVar p ch = do

  waitForTick tickChan

  evs <- takeEvents queueVar
  mapM_ (processPlayerEvent sysBus p ch) evs

  reportTickFinished tickChan

  playerWorker sysBus tickChan queueVar p ch

processPlayerEvent
  :: SystemBus
  -> Pos
  -> Char
  -> SystemEvent
  -> GameIO ()
processPlayerEvent sysBus p ch PopulateCellDescriptionEvent =
  publishEvent sysBus $ FieldIconEvent p ch
processPlayerEvent sysBus p ch PlayerInputInvitedEvent = do
  line <- withInputInvitation "Type your command:"
  publishEvent sysBus $ PlayerInputEvent p line
processPlayerEvent _ _ _ _ = pure ()

performPlayerCommand
  :: SystemBus
  -> PlayerPos
  -> PlayerCommand
  -> GameIO ()
performPlayerCommand sysBus pos (PlayerCommand mbDir actorActions) = do
  let acts = Map.toList actorActions
  case mbDir of
    Nothing  -> mapM_ (performActorAction pos) acts
    Just dir -> mapM_ (performActorAction (movePos pos dir)) acts
  where
    performActorAction pos' (_, act) = act sysBus pos'

parsePlayerCommand
  :: String
  -> GameActions
  -> GameIO (Either String PlayerCommand)
parsePlayerCommand line actions = do

  let ws = P.words line

  case ws of
    [] -> pure $ Left $ "Empty command: " <> line
    (cmd : rest) -> do
      case Map.lookup cmd actions of
        Nothing                -> pure $ Left $ "Command not found: " <> cmd
        Just (isDirected, act) -> do
          case (isDirected, rest) of
            (False, _)        -> pure $ Right $ PlayerCommand Nothing act
            (True, [])        -> pure $ Left $ "Directed command lacks direction: " <> line
            (True, (arg : _)) ->
              case readMaybe arg of
                Nothing  -> pure $ Left $ "Direction parse error: " <> arg
                Just dir -> pure $ Right $ PlayerCommand (Just dir) act
