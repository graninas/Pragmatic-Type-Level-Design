{-# LANGUAGE AllowAmbiguousTypes #-}

module Minefield.Game.Actor where

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


createActor
  :: SystemBus
  -> ((Int, Int), (ObjectType, Char))
  -> GameIO ((Int, Int), Actor)
createActor sysBus (p, (oType, ch)) = do
  inVar  <- newEmptyMVar
  outVar <- newEmptyMVar
  let tickChan = Channel inVar outVar

  queueVar <- createQueueVar

  -- TODO: FIXME: Hardcoded player symbol
  (threadId, subCond) <- case ch of
    '@' -> do
      tId <- forkIO (playerWorker sysBus tickChan queueVar p ch)
      let sub ev =
            isPlayerInputInvitedEvent ev
            || isPopulateCellDescriptionEvent ev
      pure (tId, sub)

    _   -> do
      tId <- forkIO (actorWorker tickChan queueVar oType p ch)
      let sub ev =
            isPopulateCellDescriptionEvent ev
            || isActorRequestEvent ev
      pure (tId, sub)

  subscribeRecipient sysBus $ Subscription subCond queueVar

  pure (p, Actor threadId tickChan queueVar)
  where
    actorWorker
      :: TickChannel
      -> EventQueueVar
      -> ObjectType
      -> (Int, Int)
      -> Char
      -> GameIO ()
    actorWorker tickChan queueVar oType p ch = do

      waitForTick tickChan

      evs <- takeEvents queueVar
      mapM_ (processActorEvent sysBus oType p ch) evs

      reportTickFinished tickChan

      actorWorker tickChan queueVar oType p ch

processActorEvent
  :: SystemBus
  -> ObjectType
  -> Pos
  -> Char
  -> SystemEvent
  -> GameIO ()
processActorEvent sysBus _ p ch PopulateCellDescriptionEvent =
  publishEvent sysBus $ FieldIconEvent p ch
processActorEvent sysBus ot p ch (ActorRequestEvent oType pos ev)
  | ot == oType && pos == p = do
    error "Match!"
  | otherwise = pure ()
processActorEvent _ _ _ _ _ = error "Event not supported"
