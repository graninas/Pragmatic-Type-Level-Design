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
    oInfo <- evalIO () GetObjectInfo $ Proxy @t

    tickChan <- createStepChannel
    queueVar <- createQueueVar

    let turns = fromIntegral $ natVal $ Proxy @turns
    let ticksToLive = (ticksInTurn * turns) - 1

    -- TODO: objectID
    -- TODO: lens or dot syntax
    let (i, icons) = oiIcons oInfo
    let oInfo' = oInfo {oiIcons = (i, icons), oiPos = Just pos}

    obj <- TimerBombObject
      <$> newIORef oInfo'
      <*> newIORef (TimerBombTicking ticksToLive)

    tId <- forkIO $ actorWorker tickChan queueVar
                  $ processTimerBombEvent sysBus obj
    let sub ev =
          isPopulateIconEvent ev
          || isGameFlowEvent ev
          || isObjectRequestEvent ev

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
  let oInfRef  = tboObjectInfoRef obj
  oInfo <- readIORef oInfRef
  state <- readIORef stateRef

  case state of
    TimerBombTicking ticksLeft
      | ticksLeft > 0 -> do
          writeIORef stateRef $ TimerBombTicking $ ticksLeft - 1

          when (ticksLeft `mod` ticksInTurn == 0) $ do
              let icon = head $ show $ ticksLeft `div` ticksInTurn
              let ovhIcon = OverhaulIcon Nothing icon Nothing
              let (i, icons) = oiIcons oInfo
              writeIORef oInfRef $ oInfo {oiIcons = (i, [ovhIcon])}

          publishEvent sysBus $ DebugMessageEvent
            $ "[" <> show tick <> "] TimerBomb ticking; ticks left: " <> show ticksLeft

      | otherwise -> do
          -- this is a new tick, and it should be counted as happened.
          writeIORef stateRef $ TimerBombExplosion $ ticksInTurn - 1
          let (i, _) = oiIcons oInfo
          writeIORef oInfRef $ oInfo {oiIcons = (i, explosionIcons)}

          publishEvent sysBus $ DebugMessageEvent
            $ "[" <> show tick <> "] TimerBomb explosion started"

          tickOverhaulIcons oInfRef
          makeExplosion sysBus obj

    TimerBombExplosion ticksLeft
      | ticksLeft > 0 -> do
          writeIORef stateRef $ TimerBombExplosion $ ticksLeft - 1

          publishEvent sysBus $ DebugMessageEvent
            $ "[" <> show tick <> "] TimerBomb explosion; ticks left: " <> show ticksLeft

          tickOverhaulIcons oInfRef

      | otherwise -> do
          writeIORef stateRef TimerBombDead

          publishEvent sysBus $ DebugMessageEvent
            $ "[" <> show tick <> "] TimerBomb dead"

          disableObject oInfRef

    TimerBombDead -> pure ()

processTimerBombEvent sysBus obj commonEv =
  processCommonEvent sysBus (tboObjectInfoRef obj) commonEv

-- TODO: explosion
makeExplosion sysBus obj = pure ()
