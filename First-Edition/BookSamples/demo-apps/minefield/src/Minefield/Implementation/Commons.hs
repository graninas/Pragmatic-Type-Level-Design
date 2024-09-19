module Minefield.Implementation.Commons where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.System.Types
import Minefield.Core.System.Event


disableObject :: IORef ObjectInfo -> IO ()
disableObject oInfRef = do
  oInf <- readIORef oInfRef
  writeIORef oInfRef $ oInf { oiEnabled = False }

tickOverhaulIcons :: IORef ObjectInfo -> IO ()
tickOverhaulIcons oInfRef = do
  oInf <- readIORef oInfRef
  let (i, icons) = oiIcons oInf
  case icons of
    (OverhaulIcon iId icon (Just 0) : rest) ->
      writeIORef oInfRef $ oInf { oiIcons = (i, rest) }

    (OverhaulIcon iId icon (Just 1) : rest) ->
      writeIORef oInfRef $ oInf { oiIcons = (i, rest) }

    (OverhaulIcon iId icon (Just n) : rest) -> do
      let icons' = OverhaulIcon iId icon (Just $ n - 1) : rest
      writeIORef oInfRef $ oInf { oiIcons = (i, icons') }
    _ -> pure ()


-- Common effects

disableBombEffect :: ActorAction
disableBombEffect sysBus oType pos = do
  publishEvent sysBus
     $ ObjectRequestEvent oType pos
     $ AddOverhaulIcon
     $ OverhaulIcon Nothing '!' Nothing
  publishEvent sysBus
     $ ObjectRequestEvent oType pos
     $ SetEnabled False

-- Common events

processCommonEvent
  :: SystemBus
  -> IORef ObjectInfo
  -> SystemEvent
  -> GameIO ()

-- Populate cell icon event
processCommonEvent sysBus oInfRef PopulateIconEvent = do
  oInf <- readIORef oInfRef

  when (oiEnabled oInf) $ do
    let icon = case oiIcons oInf of
                (_, oi : _ ) -> ovhIcon oi
                (i, [])      -> i

    let pos = case oiPos oInf of
                Nothing -> (-1, -1)
                Just pos -> pos

    publishEvent sysBus $ FieldIconEvent pos icon

-- Object request event
processCommonEvent sysBus oInfRef (ObjectRequestEvent oType pos ev) = do
  oInf <- readIORef oInfRef
  let curOType = oiObjectType oInf
  let mbCurPos = oiPos oInf
  let match = curOType == oType && mbCurPos == Just pos
  if match
    then processObjectRequestEvent sysBus oInfRef ev
    else pure ()

processCommonEvent _ _ (TurnEvent _) = pure ()
processCommonEvent _ _ (TickEvent _) = pure ()
processCommonEvent _ _ _ = error "Common request not implemented"

processObjectRequestEvent
  :: SystemBus
  -> IORef ObjectInfo
  -> ObjectRequestEvent
  -> GameIO ()
processObjectRequestEvent sysBus oInfRef ev = do
  case ev of
    AddOverhaulIcon oi -> do
      oInf <- readIORef oInfRef

      let (i, icons) = oiIcons oInf
      let icons' = oi : icons

      writeIORef oInfRef $ oInf {oiIcons = (i, icons')}

    SetEnabled en -> do
      oInf <- readIORef oInfRef
      writeIORef oInfRef $ oInf {oiEnabled = en}
