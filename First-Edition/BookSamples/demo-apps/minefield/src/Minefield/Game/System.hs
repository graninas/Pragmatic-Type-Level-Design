module Minefield.Game.System where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Types
import Minefield.Core.Interface
import Minefield.Game.Types

import GHC.TypeLits


createSystemBus :: IO SystemBus
createSystemBus = SystemBus
  <$> newMVar []
  <*> newMVar []

subscribeRecipient
  :: SystemBus
  -> Subscription
  -> IO ()
subscribeRecipient (SystemBus _ subsVar) sub = do
  subs <- takeMVar subsVar
  putMVar subsVar $ sub : subs

publishEvent :: SystemBus -> SystemEvent -> IO ()
publishEvent (SystemBus evsVar _) ev = do
  evs <- takeMVar evsVar
  putMVar evsVar $ ev : evs

distributeEvents :: SystemBus -> IO ()
distributeEvents (SystemBus evsVar subsVar) = do
  evs <- takeMVar evsVar

  subs <- readMVar subsVar
  mapM_ (\ev -> mapM_ (relayEvent ev) subs) evs

  putMVar evsVar []
  where
    relayEvent :: SystemEvent -> Subscription -> IO ()
    relayEvent ev (Subscription cond queueVar)
      | cond ev = pushEvent ev queueVar
      | otherwise = pure ()
    pushEvent :: SystemEvent -> EventQueueVar -> IO ()
    pushEvent ev queueVar = do
      evs <- takeMVar queueVar
      putMVar queueVar $ ev : evs

extractEvents :: EventQueueVar -> IO [SystemEvent]
extractEvents eqVar = do
  evs <- takeMVar eqVar
  putMVar eqVar []
  pure evs

dropEvents :: EventQueueVar -> IO ()
dropEvents eqVar = do
  _ <- takeMVar eqVar
  putMVar eqVar []

createStepChannel :: IO StepChannel
createStepChannel = do
  inVar  <- newEmptyMVar
  outVar <- newEmptyMVar
  pure $ Channel inVar outVar

waitForStep :: StepChannel -> IO ()
waitForStep (Channel inVar _) = takeMVar inVar

reportStepFinished :: StepChannel -> IO ()
reportStepFinished (Channel _ outVar) = putMVar outVar ()

sendStep :: StepChannel -> IO ()
sendStep (Channel inVar _) = putMVar inVar ()

waitForFinishedStep :: StepChannel -> IO ()
waitForFinishedStep (Channel _ outVar) = takeMVar outVar

createQueueVar :: IO EventQueueVar
createQueueVar = newMVar []

isPlayerInputEvent :: SystemEvent -> Bool
isPlayerInputEvent (PlayerInputEvent _ _) = True
isPlayerInputEvent _ = False

isPlayerInputInvitedEvent :: SystemEvent -> Bool
isPlayerInputInvitedEvent PlayerInputInvitedEvent = True
isPlayerInputInvitedEvent _ = False

isFieldIconEvent :: SystemEvent -> Bool
isFieldIconEvent (FieldIconEvent _ _) = True
isFieldIconEvent _ = False

isPopulateCellDescriptionEvent :: SystemEvent -> Bool
isPopulateCellDescriptionEvent PopulateCellDescriptionEvent = True
isPopulateCellDescriptionEvent _ = False

isObjectRequestEvent :: SystemEvent -> Bool
isObjectRequestEvent (ObjectRequestEvent _ _ _) = True
isObjectRequestEvent _ = False

isTickEvent :: SystemEvent -> Bool
isTickEvent TickEvent = True
isTickEvent _ = False

isTurnEvent :: SystemEvent -> Bool
isTurnEvent TurnEvent = True
isTurnEvent _ = False


movePos :: Pos -> Direction -> Pos
movePos (x, y) U = (x, y-1)
movePos (x, y) D = (x, y+1)
movePos (x, y) L = (x-1, y)
movePos (x, y) R = (x+1, y)
movePos pos DL = movePos (movePos pos L) D
movePos pos UR = movePos (movePos pos R) U
movePos pos UL = movePos (movePos pos L) U
movePos pos DR = movePos (movePos pos R) D


actorWorker
  :: StepChannel
  -> EventQueueVar
  -> (SystemEvent -> GameIO ())
  -> GameIO ()
actorWorker stepChan queueVar processActorEvent = forever $ do
  waitForStep stepChan
  evs <- extractEvents queueVar
  mapM_ processActorEvent evs
  reportStepFinished stepChan

