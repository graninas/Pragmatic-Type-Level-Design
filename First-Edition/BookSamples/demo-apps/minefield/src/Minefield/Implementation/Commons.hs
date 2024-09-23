module Minefield.Implementation.Commons where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.System.Types
import Minefield.Core.System.Event


disableObject :: IORef ObjectInfo -> IO ()
disableObject oInfoRef = do
  oInfo <- readIORef oInfoRef
  writeIORef oInfoRef $ oInfo { oiEnabled = False }

addOverhaulIcon :: IORef ObjectInfo -> OverhaulIcon -> IO ()
addOverhaulIcon oInfoRef ovhIcon = do
  oInfo <- readIORef oInfoRef
  let (i, icons) = oiIcons oInfo
  writeIORef oInfoRef $ oInfo {oiIcons = (i, ovhIcon : icons)}

tickOverhaulIcons :: IORef ObjectInfo -> IO ()
tickOverhaulIcons oInfoRef = do
  oInfo <- readIORef oInfoRef
  let (i, icons) = oiIcons oInfo
  case icons of
    (OverhaulIcon iId icon (Just 0) : rest) ->
      writeIORef oInfoRef $ oInfo { oiIcons = (i, rest) }

    (OverhaulIcon iId icon (Just 1) : rest) ->
      writeIORef oInfoRef $ oInfo { oiIcons = (i, rest) }

    (OverhaulIcon iId icon (Just n) : rest) -> do
      let icons' = OverhaulIcon iId icon (Just $ n - 1) : rest
      writeIORef oInfoRef $ oInfo { oiIcons = (i, icons') }
    _ -> pure ()


-- Common effects

disarmBombEffect :: ActorAction
disarmBombEffect sysBus oType pos = do
  publishEvent sysBus
     $ ObjectRequestEvent oType pos
     $ SetDisarmed True

-- Common events

processCommonEvent
  :: SystemBus
  -> IORef ObjectInfo
  -> Pos
  -> SystemEvent
  -> GameIO ()

-- Populate cell icon event
processCommonEvent sysBus oInfoRef objPos PopulateIconEvent = do
  oInfo <- readIORef oInfoRef

  when (oiEnabled oInfo) $ do
    let icon = case oiIcons oInfo of
                (_, oi : _ ) -> ovhIcon oi
                (i, [])      -> i

    publishEvent sysBus $ FieldIconEvent objPos icon

-- Object request event
processCommonEvent sysBus oInfoRef objPos (ObjectRequestEvent oType pos request) = do
  match <- eventTargetMatch oInfoRef objPos oType pos
  when match
    $ processObjectRequest sysBus oInfoRef request

processCommonEvent _ _ _ (TurnEvent _) = pure ()
processCommonEvent _ _ _ (TickEvent _) = pure ()
processCommonEvent _ _ _ _ = error "Common event not implemented"

processObjectRequest
  :: SystemBus
  -> IORef ObjectInfo
  -> ObjectRequest
  -> GameIO ()
processObjectRequest sysBus oInfoRef request = do
  case request of
    AddOverhaulIcon oi -> do
      oInfo <- readIORef oInfoRef

      let (i, icons) = oiIcons oInfo
      let icons' = oi : icons

      writeIORef oInfoRef $ oInfo {oiIcons = (i, icons')}

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
  pure $ curOType == oType && objPos == pos
