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
    let ticksToLive = ticksInTurn * turns

    -- TODO: objectID

    let countDownIcons =
          [ OverhaulIcon Nothing (head $ show n) (Just (ticksInTurn - 1))
          | n <- [turns..1]
          ]
    let icons' = countDownIcons <> explosionIcons

    -- TODO: lens or dot syntax
    let (i, icons) = oiIcons oInfo
    let oInfo' = oInfo {oiIcons = (i, icons'), oiPos = Just pos}

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
processTimerBombEvent sysBus obj (TickEvent _) = do
  let stateRef = tboStateRef obj
  let oInfRef  = tboObjectInfoRef obj

  state <- readIORef stateRef

  case state of
    TimerBombTicking ticksLeft
      | ticksLeft > 0 -> do
          writeIORef stateRef $ TimerBombTicking $ ticksLeft - 1
          tickOverhaulIcons oInfRef
      | otherwise -> do
          writeIORef stateRef $ TimerBombExplosion ticksInTurn
          tickOverhaulIcons oInfRef
          makeExplosion sysBus obj

    TimerBombExplosion ticksLeft
      | ticksLeft > 0 -> do
          writeIORef stateRef $ TimerBombExplosion $ ticksLeft - 1
          tickOverhaulIcons oInfRef
      | otherwise -> do
          writeIORef stateRef TimerBombDead
          tickOverhaulIcons oInfRef
          disableObject oInfRef

    TimerBombDead -> pure ()

processTimerBombEvent sysBus obj commonEv =
  processCommonEvent sysBus (tboObjectInfoRef obj) commonEv

-- TODO: explosion
makeExplosion sysBus obj = pure ()
