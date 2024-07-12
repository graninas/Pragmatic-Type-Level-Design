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
data SystemBus = SystemBus
  { sbEventsVar :: MVar [SystemEvent]
  }

data SystemEvent
  = CellDescriptionEvent (Int, Int) Char
  | PlayerInputInvitedEvent
  | PlayerInputEvent Text
  deriving (Show, Eq, Ord)

createSystemBus :: IO SystemBus
createSystemBus = SystemBus <$> newMVar []

publishSystemEvent :: SystemBus -> SystemEvent -> IO ()
publishSystemEvent (SystemBus evsVar) ev = do
  evs <- takeMVar evsVar
  putMVar evsVar $ ev : evs

readEvents :: SystemBus -> IO [SystemEvent]
readEvents (SystemBus evsVar) = fromJust <$> tryReadMVar evsVar

dropEvents :: SystemBus -> IO ()
dropEvents (SystemBus evsVar) = do
  _ <- takeMVar evsVar
  putMVar evsVar []

isPlayerInputEvent :: SystemEvent -> Bool
isPlayerInputEvent (PlayerInputEvent _) = True
isPlayerInputEvent _ = False

isPlayerInputInvitedEvent :: SystemEvent -> Bool
isPlayerInputInvitedEvent PlayerInputInvitedEvent = True
isPlayerInputInvitedEvent _ = False
