{-# LANGUAGE UndecidableInstances #-}

module Minefield.Implementation.SystemNouns.Player.Implementation where

import CPrelude

import TypeLevelDSL.Eval

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.Defaults
import Minefield.Core.Commons
import Minefield.Core.System.Types
import Minefield.Core.System.Actor
import Minefield.Core.System.Event

import Minefield.Implementation.SystemNouns.Player.Def
import Minefield.Implementation.SystemNouns.Player.Template
import Minefield.Implementation.SystemNouns.Player.Object
import Minefield.Implementation.Commons
import Minefield.Implementation.Materialization

import Minefield.App.UI

import GHC.TypeLits


-- Player instantiation

-- -- Instantiate actor

instance
  ( t ~ PlayerDef i ot
  , EvalIO () GetObjectInfo t ObjectInfo
  ) =>
  EvalIO (SystemBus, Pos) MakeActor (PlayerDef i ot) Actor where
  evalIO (sysBus, pos) _ _ = do
    stepChan <- createStepChannel
    queueVar <- createQueueVar

    oInfo <- evalIO () GetObjectInfo $ Proxy @t
    let oInfo' = oInfo { oiPos = Just pos }
    obj <- PlayerObject
      <$> newIORef oInfo'

    tId <- forkIO $ actorWorker stepChan queueVar
                  $ processPlayerEvent sysBus obj
    let sub ev =
          isPopulateIconEvent ev
          || isGameFlowEvent ev
          || isObjectRequestEvent ev
          || isPlayerInputInvitedEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId stepChan queueVar

processPlayerEvent
  :: SystemBus
  -> PlayerObject
  -> SystemEvent
  -> GameIO ()
processPlayerEvent sysBus obj PlayerInputInvitedEvent = do
  oInf <- readIORef $ poObjectInfoRef obj
  case oiPos oInf of
    Nothing -> pure ()      -- TODO: error here?
    Just pos -> do
      line <- withInputInvitation "Type your command:"
      publishEvent sysBus $ PlayerInputEvent pos line
processPlayerEvent sysBus obj commonEv =
  processCommonEvent sysBus (poObjectInfoRef obj) commonEv