module Minefield.Implementation.Commons where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.Defaults
import Minefield.Core.Commons
import Minefield.Core.System.Types
import Minefield.Core.System.Event


disableObject :: IORef ObjectInfo -> IO ()
disableObject oInfoRef = do
  oInfo <- readIORef oInfoRef
  writeIORef oInfoRef $ oInfo { oiEnabled = False }

setIcon :: IORef ObjectInfo -> Icon -> IO ()
setIcon oInfoRef icon = do
  oInfo <- readIORef oInfoRef
  let (_, ovhIcons) = oiIcons oInfo
  writeIORef oInfoRef $ oInfo {oiIcons = (icon, ovhIcons)}

appendIconBatch :: IORef ObjectInfo -> OverhaulIconBatch -> IO ()
appendIconBatch oInfoRef batch = do
  oInfo    <- readIORef oInfoRef
  batchRef <- newIORef batch
  let (i, batchRefs) = oiIcons oInfo
  writeIORef oInfoRef
    $ oInfo {oiIcons = (i, batchRef : batchRefs)}


tickOverhaulIcons :: IORef ObjectInfo -> IO ()
tickOverhaulIcons oInfoRef = do
  oInfo <- readIORef oInfoRef
  let (i, batchRefs) = oiIcons oInfo

  forM_ batchRefs $ \batchRef -> do
    OverhaulIconBatch icons <- readIORef batchRef
    let icons' = OverhaulIconBatch $ case icons of
          (OverhaulIcon iId icon (Just 0) : rest) -> rest
          (OverhaulIcon iId icon (Just 1) : rest) -> rest
          (OverhaulIcon iId icon (Just n) : rest) ->
            OverhaulIcon iId icon (Just $ n - 1) : rest
    writeIORef batchRef $ OverhaulIconBatch icons'

-- Common effects

disarmBombEffect :: ActorAction
disarmBombEffect sysBus oType pos = do
  publishEvent sysBus
     $ ActorRequestEvent oType pos
     $ SetDisarmed True

-- Common events

processCommonEvent
  :: SystemBus
  -> IORef ObjectInfo
  -> Pos
  -> SystemEvent
  -> GameIO ()

-- Populate cell icon event
processCommonEvent sysBus oInfoRef objPos PopulateIconRequestEvent = do
  oInfo <- readIORef oInfoRef

  when (oiEnabled oInfo) $ do
    icon <- case oiIcons oInfo of
      (i, [])           -> pure i
      (i, batchRef : _) -> do
        OverhaulIconBatch icons <- readIORef batchRef
        case icons of
          []         -> pure i
          (ovhI : _) -> pure $ ovhIcon ovhI

    publishEvent sysBus $ IconEvent objPos icon

-- Object request event
processCommonEvent sysBus oInfoRef objPos (ActorRequestEvent oType pos request) = do
  match <- eventTargetMatch oInfoRef objPos oType pos
  when match
    $ processActorRequest sysBus oInfoRef request

processCommonEvent _ _ _ (TurnEvent _) = pure ()
processCommonEvent _ _ _ (TickEvent _) = pure ()
processCommonEvent _ _ _ _ = error "Common event not implemented"

processActorRequest
  :: SystemBus
  -> IORef ObjectInfo
  -> ActorRequest
  -> GameIO ()
processActorRequest sysBus oInfoRef request = do
  case request of
    SetEnabled en -> do
      oInfo <- readIORef oInfoRef
      writeIORef oInfoRef $ oInfo {oiEnabled = en}

    _ -> error "Common request not implemented."

eventTargetMatch
  :: IORef ObjectInfo
  -> Pos
  -> ObjectType
  -> Pos
  -> GameIO Bool
eventTargetMatch oInfoRef objPos oType pos = do
  oInfo <- readIORef oInfoRef
  let curOType = oiObjectType oInfo
  pure $ (curOType == oType || oType == "") && objPos == pos


type ExplosionPower = Int

sendExplosionRequests
  :: SystemBus
  -> ExplosionPower
  -> Pos
  -> GameIO ()
sendExplosionRequests sysBus p pos = do
  let explosionRange = case p of
        1 -> []
        2 -> neighboursLvl2 pos
        3 -> neighboursLvl2 pos <> neighboursLvl3 pos
  mapM_ (\p -> publishEvent sysBus
                  $ ActorRequestEvent anyObjectType p SetExplosion
        ) explosionRange
