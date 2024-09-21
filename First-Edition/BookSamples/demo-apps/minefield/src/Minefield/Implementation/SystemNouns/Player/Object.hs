module Minefield.Implementation.SystemNouns.Player.Object where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object


-- | Player runtime instance.
data PlayerObject = PlayerObject
  { poObjectInfoRef :: IORef ObjectInfo
  , poPos           :: IORef Pos
  }
