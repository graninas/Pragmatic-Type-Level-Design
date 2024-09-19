module Minefield.Core.System.Event where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.System.Types

import GHC.TypeLits


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

isPlayerInputEvent :: SystemEvent -> Bool
isPlayerInputEvent (PlayerInputEvent _ _) = True
isPlayerInputEvent _ = False

isPlayerInputInvitedEvent :: SystemEvent -> Bool
isPlayerInputInvitedEvent PlayerInputInvitedEvent = True
isPlayerInputInvitedEvent _ = False

isFieldIconEvent :: SystemEvent -> Bool
isFieldIconEvent (FieldIconEvent _ _) = True
isFieldIconEvent _ = False

isPopulateIconEvent :: SystemEvent -> Bool
isPopulateIconEvent PopulateIconEvent = True
isPopulateIconEvent _ = False

isObjectRequestEvent :: SystemEvent -> Bool
isObjectRequestEvent (ObjectRequestEvent _ _ _) = True
isObjectRequestEvent _ = False

isDebugMessageEvent :: SystemEvent -> Bool
isDebugMessageEvent (DebugMessageEvent _) = True
isDebugMessageEvent _ = False

isTickEvent :: SystemEvent -> Bool
isTickEvent (TickEvent _) = True
isTickEvent _ = False

isTurnEvent :: SystemEvent -> Bool
isTurnEvent (TurnEvent _) = True
isTurnEvent _ = False

isGameFlowEvent :: SystemEvent -> Bool
isGameFlowEvent ev
  = isTickEvent ev
  || isTurnEvent ev
