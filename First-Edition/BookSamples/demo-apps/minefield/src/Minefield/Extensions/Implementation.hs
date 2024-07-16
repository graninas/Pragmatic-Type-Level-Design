module Minefield.Extensions.Implementation where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Types
import Minefield.Core.Interface
import Minefield.Core.Object

import Minefield.Game.Types
import Minefield.Game.System
import Minefield.Game.UI

import Minefield.Extensions.Materialization
import Minefield.Extensions.Nouns.Landmine
import Minefield.Extensions.Nouns.Player
import Minefield.Extensions.Nouns.EmptyCell
import Minefield.Extensions.Verbs.PutFlag

import GHC.TypeLits


-- Landmine

instance
  ( KnownSymbol ot

  -- For some reason, this syntax is not universal.
  -- It prevents other objects from being accepted
  -- when unwrapping from ObjectWrapper.
  -- , obj ~ LandmineImpl i ot p
  ) =>
  Eval () MakeActorAction
       (ObjAct (LandmineImpl i ot p) PutFlagImpl)
       (ObjectType, ActorAction) where
  eval () _ _ = do
    let oType = symbolVal $ Proxy @ot

    let act = \sysBus pos -> do
          publishEvent sysBus
            $ ObjectRequestEvent oType pos
            $ AddOverhaulIcon
            $ OverhaulIcon '🚩' (TurnsCount (-1)) (TicksCount 0)
          publishEvent sysBus
            $ ObjectRequestEvent oType pos
            $ SetEnabled False

    pure (oType, act)

instance
  ( KnownSymbol i
  , KnownSymbol ot
  , KnownNat p
  ) =>
  Eval (SystemBus, Pos) MakeActor (LandmineImpl i ot p) Actor where
  eval (sysBus, pos) _ _ = do
    stepChan <- createStepChannel
    queueVar <- createQueueVar

    let icon  = head $ symbolVal $ Proxy @i
    let oType = symbolVal $ Proxy @ot
    let p     = fromIntegral $ natVal $ Proxy @p

    let oInfo = ObjectInfo icon pos oType True []
    obj <- LandmineObject
      <$> newIORef oInfo
      <*> newIORef p

    tId <- forkIO $ actorWorker stepChan queueVar
                  $ processLandmineEvent sysBus obj
    let sub ev =
          isPopulateCellDescriptionEvent ev
          || isObjectRequestEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId stepChan queueVar

processLandmineEvent
  :: SystemBus
  -> LandmineObject
  -> SystemEvent
  -> GameIO ()
processLandmineEvent sysBus obj commonEv =
  processCommonEvent sysBus (loObjectInfoRef obj) commonEv


-- Player

instance
  ( KnownSymbol i
  , KnownSymbol ot
  ) =>
  Eval (SystemBus, Pos) MakeActor (PlayerImpl i ot) Actor where
  eval (sysBus, pos) _ _ = do
    stepChan <- createStepChannel
    queueVar <- createQueueVar

    let icon  = head $ symbolVal $ Proxy @i
    let oType = symbolVal $ Proxy @ot

    let oInfo = ObjectInfo icon pos oType True []
    obj <- PlayerObject
      <$> newIORef oInfo

    tId <- forkIO $ actorWorker stepChan queueVar
                  $ processPlayerEvent sysBus obj
    let sub ev =
          isPopulateCellDescriptionEvent ev
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
  line <- withInputInvitation "Type your command:"
  publishEvent sysBus $ PlayerInputEvent (oiPos oInf) line
processPlayerEvent sysBus obj commonEv =
  processCommonEvent sysBus (poObjectInfoRef obj) commonEv

-- Empty cell

instance
  ( KnownSymbol i
  , KnownSymbol ot
  ) =>
  Eval (SystemBus, Pos) MakeActor (EmptyCellImpl i ot) Actor where
  eval (sysBus, pos) _ _ = do
    stepChan <- createStepChannel
    queueVar <- createQueueVar

    let icon  = head $ symbolVal $ Proxy @i
    let oType = symbolVal $ Proxy @ot

    let oInfo = ObjectInfo icon pos oType True []
    obj <- EmptyCellObject
            <$> newIORef oInfo

    tId <- forkIO $ actorWorker stepChan queueVar
                  $ processEmptyCellEvent sysBus obj
    let sub ev =
          isPopulateCellDescriptionEvent ev
          || isObjectRequestEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId stepChan queueVar

processEmptyCellEvent
  :: SystemBus
  -> EmptyCellObject
  -> SystemEvent
  -> GameIO ()
processEmptyCellEvent sysBus obj commonEv =
  processCommonEvent sysBus (ecoObjectInfoRef obj) commonEv


-- Common events

processCommonEvent
  :: SystemBus
  -> IORef ObjectInfo
  -> SystemEvent
  -> GameIO ()
processCommonEvent sysBus oInfRef PopulateCellDescriptionEvent = do
  oInf <- readIORef oInfRef
  let icon = case oiOverhaulIcons oInf of
              [] -> oiIcon oInf
              (i : _) -> ovhIcon i
  publishEvent sysBus
    $ FieldIconEvent (oiPos oInf) icon

processCommonEvent sysBus oInfRef (ObjectRequestEvent oType pos ev) = do
  oInf <- readIORef oInfRef
  let curOType = oiObjectType oInf
  let curPos = oiPos oInf
  let match = curOType == oType && curPos == pos
  if match
    then processObjectRequestEvent sysBus oInfRef ev
    else pure ()
processCommonEvent _ _ _ = error "Common request not implemented"


processObjectRequestEvent
  :: SystemBus
  -> IORef ObjectInfo
  -> ObjectRequestEvent
  -> GameIO ()
processObjectRequestEvent sysBus oInfRef ev = do
  case ev of
    AddOverhaulIcon i -> do
      oInf <- readIORef oInfRef
      writeIORef oInfRef
        $ oInf {oiOverhaulIcons = i : oiOverhaulIcons oInf}
    SetEnabled en -> do
      oInf <- readIORef oInfRef
      writeIORef oInfRef
        $ oInf {oiEnabled = en}
