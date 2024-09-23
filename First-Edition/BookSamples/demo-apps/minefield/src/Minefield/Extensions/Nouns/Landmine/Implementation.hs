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

    obj <- LandmineObject
      <$> newIORef oInfo
      <*> pure pos
      <*> newIORef p
      <*> newIORef LandmineActive

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
processLandmineEvent sysBus obj (ObjectRequestEvent oType pos ev) = do
  let oInfoRef = loObjectInfoRef obj
  let objPos   = loPos obj
  oInfo <- readIORef oInfoRef
  let curOType = oiObjectType oInfo
  match <- eventTargetMatch oInfoRef objPos oType pos
  when match $ do
    case ev of
      SetDisarmed en -> do
        addOverhaulIcon oInfoRef $ OverhaulIcon Nothing disarmedIcon Nothing
        writeIORef (loStateRef obj) LandmineDisarmed
      _ -> processObjectRequest sysBus oInfoRef ev

processLandmineEvent sysBus obj commonEv =
  processCommonEvent sysBus (loObjectInfoRef obj) (loPos obj) commonEv

