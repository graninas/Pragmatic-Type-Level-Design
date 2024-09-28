{-# LANGUAGE UndecidableInstances #-}

module Minefield.Extensions.Nouns.TimerBomb.Implementation where

import CPrelude

import TypeLevelDSL.Eval

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.Defaults
import Minefield.Core.Commons
import Minefield.Core.System.Types
import Minefield.Core.System.Actor
import Minefield.Core.System.Event

import Minefield.Extensions.Nouns.TimerBomb.Def
import Minefield.Extensions.Nouns.TimerBomb.Template
import Minefield.Extensions.Nouns.TimerBomb.Object
import Minefield.Implementation.Commons
import Minefield.Implementation.Materialization

import GHC.TypeLits


-- TimerBomb instantiation

-- -- Instantiate actor

instance
  ( t ~ TimerBombDef i ot turns
  , KnownNat turns
  , EvalIO () GetObjectInfo t ObjectInfo
  ) =>
  EvalIO (SystemBus, Pos) MakeActor (TimerBombDef i ot turns) Actor where
  evalIO (sysBus, pos) _ _ = do
    tickChan <- createStepChannel
    queueVar <- createQueueVar

    let turns = fromIntegral $ natVal $ Proxy @turns
    let ticksToLive = (ticksInTurn * turns) - 1

    oInfo <- evalIO () GetObjectInfo $ Proxy @t
    -- TODO: objectID
    obj <- TimerBombObject
      <$> newIORef oInfo
      <*> pure pos
      <*> newIORef (TimerBombTicking ticksToLive)

    tId <- forkIO $ actorWorker tickChan queueVar
                  $ processTimerBombEvent sysBus obj
    let sub ev =
          isPopulateIconRequestEvent ev
          || isGameFlowEvent ev
          || isActorRequestEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId tickChan queueVar


-- Process events

processTimerBombEvent
  :: SystemBus
  -> TimerBombObject
  -> SystemEvent
  -> GameIO ()
processTimerBombEvent sysBus obj (TurnEvent _) = pure ()
processTimerBombEvent sysBus obj (TickEvent tick) = do
  let stateRef = tboStateRef obj
  let oInfoRef = tboObjectInfoRef obj

  state <- readIORef stateRef
  case state of
    TimerBombTicking ticksLeft
      -- Ticking is happening
      | ticksLeft > 0 -> do
          writeIORef stateRef $ TimerBombTicking $ ticksLeft - 1

          when (ticksLeft `mod` ticksInTurn == 0) $ do
            let icon = head $ show $ ticksLeft `div` ticksInTurn
            setOverhaulIcon oInfoRef $ OverhaulIcon Nothing icon Nothing

      -- Ticking ended; explosion just happened
      | otherwise -> do
          -- this is a new tick, and it should be counted as happened.
          writeIORef stateRef $ TimerBombExplosion $ ticksInTurn - 1

          -- TODO: turn this object to an empty cell
          -- tmp solution:
          setOverhaulIcon oInfoRef $ OverhaulIcon Nothing ' ' Nothing

          addExplosionOverhaulIcons oInfoRef

          sendExplosionRequests sysBus 2 (tboPos obj)

    -- Explosion is happening
    TimerBombExplosion ticksLeft
      -- Explosion animation is happening
      | ticksLeft > 0 -> do
          writeIORef stateRef $ TimerBombExplosion $ ticksLeft - 1
          tickOverhaulIcons oInfoRef

      -- Explosion animation ended
      | otherwise -> do
          writeIORef stateRef TimerBombDead
          tickOverhaulIcons oInfoRef

    TimerBombDead     -> tickOverhaulIcons oInfoRef
    TimerBombDisarmed -> tickOverhaulIcons oInfoRef

processTimerBombEvent sysBus obj (ActorRequestEvent oType pos ev) = do
  let oInfoRef = tboObjectInfoRef obj
  let objPos   = tboPos obj
  let stateRef = tboStateRef obj
  match <- eventTargetMatch oInfoRef objPos oType pos
  when match $ do
    case ev of
      SetDisarmed en -> do
        setOverhaulIcon oInfoRef disarmedOverhaulIcon
        writeIORef stateRef TimerBombDisarmed
      SetExplosion -> do
        state <- readIORef stateRef
        case state of
          TimerBombDead     -> do
            --- ??????? will this work?
            addExplosionOverhaulIcons oInfoRef

          TimerBombDisarmed -> do
            --- ??????? will this work?
            addExplosionOverhaulIcons oInfoRef

          TimerBombTicking _ -> do
            writeIORef stateRef $ TimerBombExplosion ticksInTurn

            -- TODO: turn this object to an empty cell
            -- tmp solution:
            setOverhaulIcon oInfoRef $ OverhaulIcon Nothing ' ' Nothing
            addExplosionOverhaulIcons oInfoRef
            sendExplosionRequests sysBus 2 (tboPos obj)

          TimerBombExplosion _ -> do
            -- TODO: turn this object to an empty cell
            -- tmp solution:
            setOverhaulIcon oInfoRef $ OverhaulIcon Nothing ' ' Nothing
            addExplosionOverhaulIcons oInfoRef

      _ -> processActorRequest sysBus oInfoRef ev

processTimerBombEvent sysBus obj commonEv =
  processCommonEvent sysBus (tboObjectInfoRef obj) (tboPos obj) commonEv

