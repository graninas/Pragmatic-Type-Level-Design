module Minefield.Core.System where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Language

import GHC.TypeLits


-- Static infrastructure


data GetIcon


instance
  ( Eval GetIcon o Char
  ) =>
  Eval GetIcon ('ObjectWrapper o) Char where
  eval vProxy _ = eval vProxy $ Proxy @o


data Objects a

instance
  Eval GetIcon (Objects '[]) [Char] where
  eval _ _ = []

instance
  ( Eval GetIcon o Char
  , Eval GetIcon (Objects os) [Char]
  ) =>
  Eval GetIcon (Objects (o ': os)) [Char] where
  eval vProxy _ = let
    o  = eval vProxy $ Proxy @o
    os = eval vProxy $ Proxy @(Objects os)
    in o : os


-- Dynamic infrastructure

data SystemEvent
  = PlayerInputInvitedEvent
  | PlayerInputEvent Text
  | PopulateCellDescriptionsEvent
  | CellDescriptionEvent (Int, Int) Char
  deriving (Show, Eq, Ord)

type EventQueueVar = MVar [SystemEvent]

data SystemBus = SystemBus
  { sbEventsVar :: EventQueueVar
  , sbSubscriptionsVar :: MVar [Subscription]
  }

data Subscription = Subscription
  { sCondition :: SystemEvent -> Bool
  , sRecipientQueueVar :: EventQueueVar
  }

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

-- readEvents :: EventQueueVar -> IO [SystemEvent]
-- readEvents eqVar = fromJust <$> tryReadMVar eqVar

takeEvents :: EventQueueVar -> IO [SystemEvent]
takeEvents eqVar = do
  evs <- takeMVar eqVar
  putMVar eqVar []
  pure evs

-- readEvents' :: SystemBus -> IO [SystemEvent]
-- readEvents' (SystemBus eqVar) = readEvents eqVar

dropEvents :: EventQueueVar -> IO ()
dropEvents eqVar = do
  _ <- takeMVar eqVar
  putMVar eqVar []

-- dropEvents' :: SystemBus -> IO ()
-- dropEvents' (SystemBus eqVar) = dropEvents eqVar

createQueueVar :: IO EventQueueVar
createQueueVar = newMVar []

isPlayerInputEvent :: SystemEvent -> Bool
isPlayerInputEvent (PlayerInputEvent _) = True
isPlayerInputEvent _ = False

isPlayerInputInvitedEvent :: SystemEvent -> Bool
isPlayerInputInvitedEvent PlayerInputInvitedEvent = True
isPlayerInputInvitedEvent _ = False
