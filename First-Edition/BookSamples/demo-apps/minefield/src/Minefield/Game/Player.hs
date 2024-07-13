{-# LANGUAGE AllowAmbiguousTypes #-}

module Minefield.Game.Player where

import CPrelude

import Minefield.Core.System
import Minefield.Core.UI

import Minefield.Game.Types

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
