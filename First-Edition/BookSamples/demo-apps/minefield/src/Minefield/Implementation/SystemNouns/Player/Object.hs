module Minefield.Implementation.SystemNouns.Player.Object where

import CPrelude

import Minefield.Core.Object


-- | Player runtime instance.
data PlayerObject = PlayerObject
  { poObjectInfoRef :: IORef ObjectInfo
  }
