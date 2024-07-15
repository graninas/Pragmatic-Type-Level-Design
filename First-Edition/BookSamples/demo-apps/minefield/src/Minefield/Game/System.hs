module Minefield.Game.System where

import CPrelude

import Minefield.Core.Eval
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

takeEvents :: EventQueueVar -> IO [SystemEvent]
takeEvents eqVar = do
  evs <- takeMVar eqVar
  putMVar eqVar []
  pure evs

dropEvents :: EventQueueVar -> IO ()
dropEvents eqVar = do
  _ <- takeMVar eqVar
  putMVar eqVar []

waitForTick :: TickChannel -> IO ()
waitForTick (Channel inVar _) = takeMVar inVar

reportTickFinished :: TickChannel -> IO ()
reportTickFinished (Channel _ outVar) = putMVar outVar ()

sendTick :: TickChannel -> IO ()
sendTick (Channel inVar _) = putMVar inVar ()

waitForFinishedTick :: TickChannel -> IO ()
waitForFinishedTick (Channel _ outVar) = takeMVar outVar

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
