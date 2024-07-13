module Minefield.Game.Player where

import CPrelude

import Minefield.Game.Types
import Minefield.Game.System
import Minefield.Game.UI

import qualified Data.List as L
import qualified Data.Map as Map
import System.Random (randomRIO)


playerWorker
  :: SystemBus
  -> TickChannel
  -> EventQueueVar
  -> (Int, Int)
  -> Char
  -> IO ()
playerWorker sysBus tickChan queueVar p ch = do

  waitForTick tickChan

  evs <- takeEvents queueVar
  mapM_ (processPlayerEvent sysBus p ch) evs

  reportTickFinished tickChan

  playerWorker sysBus tickChan queueVar p ch

processPlayerEvent sysBus p ch PopulateCellDescriptionEvent =
  publishEvent sysBus $ FieldEvent p ch
processPlayerEvent sysBus p ch PlayerInputInvitedEvent = do
  line <- withInputInvitation "Type your command:"
  publishEvent sysBus $ PlayerInputEvent line
processPlayerEvent _ _ _ _ = pure ()


parsePlayerCommand
  :: PlayerCommands
  -> Text
  -> Maybe PlayerCommand
parsePlayerCommand cmds line = Nothing
