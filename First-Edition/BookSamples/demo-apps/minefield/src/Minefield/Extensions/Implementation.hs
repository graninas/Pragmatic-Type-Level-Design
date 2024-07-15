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



instance
  ( obj ~ LandmineImpl ch ot p
  , Eval () GetObjectType obj String
  ) =>
  Eval () MakeActorAction
       (ObjAct obj PutFlagImpl)
       (ObjectType, ActorAction) where
  eval () _ _ = do
    oType <- eval () (Proxy @GetObjectType) $ Proxy @obj

    let act = \sysBus pos -> do
          publishEvent sysBus $ ActorRequestEvent oType pos $ AddOverhaulIcon '🚩'
          publishEvent sysBus $ ActorRequestEvent oType pos $ SetEnabled False

    pure (oType, act)


-- Landmine

instance
  ( KnownSymbol i
  , KnownSymbol ot
  , KnownNat p
  ) =>
  Eval (SystemBus, Pos) MakeActor (LandmineImpl i ot p) Actor where
  eval (sysBus, pos) _ _ = do
    tickChan <- createTickChannel
    queueVar <- createQueueVar

    let icon  = head $ symbolVal $ Proxy @i
    let oType = symbolVal $ Proxy @ot
    let p     = fromIntegral $ natVal $ Proxy @p

    let oInfo = ObjectInfo icon pos oType
    obj <- LandmineObject
      <$> newIORef oInfo
      <*> newIORef p

    tId <- forkIO $ actorWorker tickChan queueVar
                  $ processLandmineEvent sysBus obj
    let sub ev =
          isPopulateCellDescriptionEvent ev
          || isActorRequestEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId tickChan queueVar

processLandmineEvent
  :: SystemBus
  -> LandmineObject
  -> SystemEvent
  -> GameIO ()
processLandmineEvent sysBus obj PopulateCellDescriptionEvent = do
  oInf <- readIORef $ loObjectInfoRef obj
  publishEvent sysBus $ FieldIconEvent (oiPos oInf) (oiIcon oInf)
processLandmineEvent sysBus obj (ActorRequestEvent oType pos ev) = do
  oInf <- readIORef $ loObjectInfoRef obj
  let curOType = oiObjectType oInf
  let curPos = oiPos oInf
  if curOType == oType && curPos == pos
    then error $ "Landmine match! " <> show pos
    else pure ()
processLandmineEvent _ _ _ = error "Landmine event not supported"


-- Player

instance
  ( KnownSymbol i
  , KnownSymbol ot
  ) =>
  Eval (SystemBus, Pos) MakeActor (PlayerImpl i ot) Actor where
  eval (sysBus, pos) _ _ = do
    tickChan <- createTickChannel
    queueVar <- createQueueVar

    let icon  = head $ symbolVal $ Proxy @i
    let oType = symbolVal $ Proxy @ot

    let oInfo = ObjectInfo icon pos oType
    obj <- PlayerObject
      <$> newIORef oInfo

    tId <- forkIO $ actorWorker tickChan queueVar
                  $ processPlayerEvent sysBus obj
    let sub ev =
          isPopulateCellDescriptionEvent ev
          || isActorRequestEvent ev
          || isPlayerInputInvitedEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId tickChan queueVar

processPlayerEvent
  :: SystemBus
  -> PlayerObject
  -> SystemEvent
  -> GameIO ()
processPlayerEvent sysBus obj PlayerInputInvitedEvent = do
  oInf <- readIORef $ poObjectInfoRef obj
  line <- withInputInvitation "Type your command:"
  publishEvent sysBus $ PlayerInputEvent (oiPos oInf) line
processPlayerEvent sysBus obj PopulateCellDescriptionEvent = do
  oInf <- readIORef $ poObjectInfoRef obj
  publishEvent sysBus $ FieldIconEvent (oiPos oInf) (oiIcon oInf)
processPlayerEvent sysBus obj (ActorRequestEvent oType pos ev) = do
  oInf <- readIORef $ poObjectInfoRef obj
  let curOType = oiObjectType oInf
  let curPos = oiPos oInf
  if curOType == oType && curPos == pos
    then error $ "Player match!" <> show pos
    else pure ()
processPlayerEvent _ _ _ = error "Player event not supported"



-- Empty cell

instance
  ( KnownSymbol i
  , KnownSymbol ot
  ) =>
  Eval (SystemBus, Pos) MakeActor (EmptyCellImpl i ot) Actor where
  eval (sysBus, pos) _ _ = do
    tickChan <- createTickChannel
    queueVar <- createQueueVar

    let icon  = head $ symbolVal $ Proxy @i
    let oType = symbolVal $ Proxy @ot

    let oInfo = ObjectInfo icon pos oType
    obj <- EmptyCellObject
            <$> newIORef oInfo

    tId <- forkIO $ actorWorker tickChan queueVar
                  $ processEmptyCellEvent sysBus obj
    let sub ev =
          isPopulateCellDescriptionEvent ev
          || isActorRequestEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId tickChan queueVar

processEmptyCellEvent
  :: SystemBus
  -> EmptyCellObject
  -> SystemEvent
  -> GameIO ()
processEmptyCellEvent sysBus obj PopulateCellDescriptionEvent = do
  oInf <- readIORef $ ecoObjectInfoRef obj
  publishEvent sysBus $ FieldIconEvent (oiPos oInf) (oiIcon oInf)
processEmptyCellEvent sysBus obj (ActorRequestEvent oType pos ev) = do
  oInf <- readIORef $ ecoObjectInfoRef obj
  let curOType = oiObjectType oInf
  let curPos = oiPos oInf
  if curOType == oType && curPos == pos
    then error $ "Empty cell match!" <> show pos
    else pure ()
processEmptyCellEvent _ _ _ = error "Empty cell event not supported"
