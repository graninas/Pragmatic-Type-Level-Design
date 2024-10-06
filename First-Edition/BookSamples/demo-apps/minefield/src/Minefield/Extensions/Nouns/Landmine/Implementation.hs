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
      <*> pure p
      <*> newIORef LandmineActive

    tId <- forkIO $ actorWorker stepChan queueVar
                  $ processLandmineEvent sysBus obj
    let sub ev =
          isPopulateIconRequestEvent ev
          || isGameFlowEvent ev
          || isActorRequestEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId stepChan queueVar

-- Process events

-- TODO: landmine logic

processLandmineEvent
  :: SystemBus
  -> LandmineObject
  -> SystemEvent
  -> GameIO ()
processLandmineEvent sysBus obj (TurnEvent _) = pure ()
processLandmineEvent sysBus obj (TickEvent tick) = do
  let stateRef = loStateRef obj
  let oInfoRef = loObjectInfoRef obj

  tickOverhaulIcons oInfoRef

  state <- readIORef stateRef
  case state of
    LandmineActive -> pure ()

    -- Explosion is happening
    LandmineExplosion ticksLeft
      -- Explosion animation is happening
      | ticksLeft > 0 ->  writeIORef stateRef $ LandmineExplosion $ ticksLeft - 1

      -- Explosion animation ended
      | otherwise -> writeIORef stateRef LandmineDead

    LandmineDead     -> pure ()
    LandmineDisarmed -> pure ()

processLandmineEvent sysBus obj (ActorRequestEvent oType pos ev) = do
  let oInfoRef = loObjectInfoRef obj
  let objPos   = loPos obj

  oInfo <- readIORef oInfoRef
  let curOType = oiObjectType oInfo
  match <- eventTargetMatch oInfoRef objPos oType pos

  pure ()
  -- when match $ do
  --   case ev of
  --     SetDisarmed en -> do
  --       setOverhaulIcon oInfoRef disarmedOverhaulIcon
  --       writeIORef (loStateRef obj) LandmineDisarmed
  --     SetExplosion -> do
  --       let stateRef = loStateRef obj
  --       state <- readIORef stateRef
  --       case state of
  --         LandmineDead     -> do
  --           addExplosionOverhaulIcons oInfoRef
  --           --- ---will this work????
  --         LandmineDisarmed -> do
  --           addExplosionOverhaulIcons oInfoRef
  --           --- ??????? will this work?
  --         LandmineActive   -> do
  --           setExplosionOverhaulIcons oInfoRef
  --           writeIORef stateRef $ LandmineExplosion ticksInTurn
  --           makeExplosion sysBus obj

  --     _ -> processActorRequest sysBus oInfoRef ev

processLandmineEvent sysBus obj commonEv =
  processCommonEvent sysBus (loObjectInfoRef obj) (loPos obj) commonEv


