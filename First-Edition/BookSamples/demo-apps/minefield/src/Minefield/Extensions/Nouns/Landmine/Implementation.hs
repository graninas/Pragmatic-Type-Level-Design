{-# LANGUAGE UndecidableInstances #-}

module Minefield.Extensions.Nouns.Landmine.Implementation where

import CPrelude

import TypeLevelDSL.Eval

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.Defaults
import Minefield.Core.Commons
import Minefield.Core.System.Types
import Minefield.Core.System.Actor
import Minefield.Core.System.Event

import Minefield.Extensions.Nouns.Landmine.Def
import Minefield.Extensions.Nouns.Landmine.Template
import Minefield.Extensions.Nouns.Landmine.Object
import Minefield.Implementation.Commons
import Minefield.Implementation.Materialization

import GHC.TypeLits


-- Landmine instantiation

-- -- Instantiate actor

instance
  ( t ~ LandmineDef i ot p
  , EvalIO () GetObjectInfo t ObjectInfo
  , KnownNat p
  ) =>
  EvalIO (SystemBus, Pos)
         MakeActor
         (LandmineDef i ot p)
         Actor where
  evalIO (sysBus, pos) _ _ = do
    stepChan <- createStepChannel
    queueVar <- createQueueVar

    let p = fromIntegral $ natVal $ Proxy @p

    oInfo <- evalIO () GetObjectInfo $ Proxy @t
    let oInfo' = oInfo { oiPos = Just pos }

    obj <- LandmineObject
      <$> newIORef oInfo'
      <*> newIORef p

    tId <- forkIO $ actorWorker stepChan queueVar
                  $ processLandmineEvent sysBus obj
    let sub ev =
          isPopulateIconEvent ev
          || isGameFlowEvent ev
          || isObjectRequestEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId stepChan queueVar

-- Process events

-- TODO: landmine logic

processLandmineEvent
  :: SystemBus
  -> LandmineObject
  -> SystemEvent
  -> GameIO ()
processLandmineEvent sysBus obj commonEv =
  processCommonEvent sysBus (loObjectInfoRef obj) commonEv

