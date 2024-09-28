{-# LANGUAGE UndecidableInstances #-}

module Minefield.Implementation.SystemNouns.EmptyCell.Implementation where

import CPrelude

import TypeLevelDSL.Eval

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.Defaults
import Minefield.Core.Commons
import Minefield.Core.System.Types
import Minefield.Core.System.Actor
import Minefield.Core.System.Event

import Minefield.Implementation.SystemNouns.EmptyCell.Def
import Minefield.Implementation.SystemNouns.EmptyCell.Template
import Minefield.Implementation.SystemNouns.EmptyCell.Object
import Minefield.Implementation.Commons
import Minefield.Implementation.Materialization

import GHC.TypeLits


-- Empty cell instantiation

-- -- Instantiate actor

instance
  ( t ~ EmptyCellDef i ot
  , EvalIO () GetObjectInfo t ObjectInfo
  ) =>
  EvalIO (SystemBus, Pos) MakeActor (EmptyCellDef i ot) Actor where
  evalIO (sysBus, pos) _ _ = do
    stepChan <- createStepChannel
    queueVar <- createQueueVar

    oInfo <- evalIO () GetObjectInfo $ Proxy @t
    obj <- EmptyCellObject
            <$> newIORef oInfo
            <*> pure pos

    tId <- forkIO $ actorWorker stepChan queueVar
                  $ processEmptyCellEvent sysBus obj
    let sub ev =
          isPopulateIconRequestEvent ev
          || isGameFlowEvent ev
          || isActorRequestEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId stepChan queueVar

-- -- Process events

processEmptyCellEvent
  :: SystemBus
  -> EmptyCellObject
  -> SystemEvent
  -> GameIO ()
processEmptyCellEvent sysBus obj (TurnEvent _) = pure ()
processEmptyCellEvent sysBus obj (TickEvent tick) = do
  tickOverhaulIcons (ecoObjectInfoRef obj)
processEmptyCellEvent sysBus obj (ActorRequestEvent oType pos ev) = do
  let oInfoRef = ecoObjectInfoRef obj
  let objPos   = ecoPos obj
  match <- eventTargetMatch oInfoRef objPos oType pos
  when match $ do
    case ev of
      SetExplosion -> addExplosionOverhaulIcons oInfoRef
      _ -> processActorRequest sysBus oInfoRef ev

processEmptyCellEvent sysBus obj commonEv =
  processCommonEvent sysBus (ecoObjectInfoRef obj) (ecoPos obj) commonEv
