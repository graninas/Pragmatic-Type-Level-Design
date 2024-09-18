module Minefield.Core.System.Actor where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.System.Types
import Minefield.Core.System.Event
import Minefield.Core.Object

import qualified Data.Map as Map


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


